{-#OPTIONS_GHC -Wall #-}
module Candid.Store
  ( Entry(..)
  , prettyEntry
  , Store
  , empty
  , byName
  , byHash
  , hashFrom
  , add
  , smush
  , smushOnce
  , expand
  , expandOnce
  ) where

import Candid.Expression
import Candid.Typecheck
import qualified Data.HashMap.Strict as HM
import qualified Blake2s1 as H

data Entry = Entry { entryName :: String
                   , entryExpr :: Expression
                   , entryHash :: H.Hash
                   } deriving (Show, Read)

prettyEntry :: Entry -> String
prettyEntry entry = "Bind Fix: " ++ entryName entry ++ "\nType: " ++ pretty [] (typeOf $ entryExpr entry) ++ "\nExpression: " ++ pretty [] (entryExpr entry) ++ "\nHash: " ++ H.toHex (entryHash entry)

type Store = HM.HashMap H.Hash Entry

empty :: Store
empty = HM.empty

-- get all the entries with name
byName :: Store -> String -> [Entry]
byName store n = Prelude.map snd $ HM.toList $ HM.filter ((n ==) . entryName) store

-- get the entry with hash
byHash :: Store -> H.Hash -> Maybe Entry
byHash store h = HM.lookup h store

-- add entryExpr to store
add :: Store -> Expression -> Either String (H.Hash, Store)
add store expr = let expr' = smush store $ typeFill (fmap entryExpr . byHash store) [] $ smush store expr
                     k = hashOf expr'
                  in case holesIn expr' of
                          [] -> Right $ (k, HM.insert k (Entry (nameOf expr) expr' k) store)
                          holes ->
                            Left $ unlines $ ("Holes in: " ++ pretty [] expr') :
                              (map ('\t' :) $ filter (/= "") holes)

hashFrom :: Entry -> Expression
hashFrom entry = Hash (typeOf $ entryExpr entry) (entryName entry) (entryHash entry)

smushOnce :: Store -> Expression -> Expression
smushOnce store expr =
  case byHash store $ hashOf expr of
       Nothing -> expr
       Just entry -> hashFrom entry

-- replace sub-entryExprs that are in store with their hashes
smush :: Store -> Expression -> Expression
smush store =
  let rec expr =
        case expr of
             Star _ -> expr
             Hole _ -> expr
             Ref ty n -> Ref (rec ty) n
             Bind b ty name inType outType -> smushOnce store $ Bind b (rec ty) name (rec inType) (rec outType)
             Apply ty function argument -> smushOnce store $ Apply (rec ty) (rec function) (rec argument)
             Hash ty name hash -> Hash (rec ty) name hash
   in rec

expandOnce :: Store -> Expression -> Expression
expandOnce store expr =
  case expr of
       Hash _ _ hash -> maybe expr (entryExpr) $ byHash store hash
       _ -> expr

-- replace sub-entryExprs that are in store with their hashes
expand :: Store -> Expression -> Expression
expand store =
  let rec expr =
        case expr of
             Star _ -> expr
             Hole _ -> expr
             Ref ty n -> Ref (rec ty) n
             Bind b ty name inType body -> Bind b (rec ty) name (rec inType) (rec body)
             Apply ty function argument -> Apply (rec ty) (rec function) (rec argument)
             Hash ty name hash -> maybe (Hash (rec ty) name hash) (rec . entryExpr) $ byHash store hash
   in rec

