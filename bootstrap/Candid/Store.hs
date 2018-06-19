{-#OPTIONS_GHC -Wall #-}
module Candid.Store
  ( Entry(..)
  , prettyEntry
  , Store
  , empty
  , byName
  , byHash
  , add
  , smush
  , expand
  ) where

import Candid.Expression
import Candid.Typecheck
import qualified Data.HashMap.Strict as HM
import qualified Blake2s1 as H

data Entry = Entry { entryName :: String
                   , entryType :: Expression
                   , entryExpr :: Expression
                   , entryHash :: H.Hash
                   } deriving (Show, Read)

prettyEntry :: Entry -> String
prettyEntry entry = "Name: " ++ entryName entry ++ "\nType: " ++ pretty [] (entryType entry) ++ "\nExpression: " ++ pretty [] (entryExpr entry) ++ "\nHash: " ++ H.toHex (entryHash entry)

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
add :: Store -> Expression -> Either TypeError (H.Hash, Store)
add store x = let x' = smush store x
                  t = typecheck (fmap entryExpr . byHash store) (fmap entryType . byHash store) False False [] x'
                  k = Candid.Expression.hash x'
               in t >>= \ty -> Right (k, HM.insert k (Entry (nameOf x) ty x' k) store)

-- replace sub-entryExprs that are in store with their hashes
smush :: Store -> Expression -> Expression
smush store = sm
  where
    lu x = let h = Candid.Expression.hash x
               in maybe x (const $ Hash (nameOf x) h) $ HM.lookup h store
    sm :: Expression -> Expression
    sm (Name nm b) = lu $ Name nm (sm b)
    sm (Pi n iT oT) = lu $ Pi n (sm iT) (sm oT)
    sm (Lambda n iT b) = lu $ Lambda n (sm iT) (sm b)
    sm (Apply f a) = lu $ Apply (sm f) (sm a)
    sm (Assert oT b) = lu $ Assert (sm oT) (sm b)
    sm x = x

-- replace hashes that are in store with their content
expand :: Store -> Expression -> Expression
expand store = ex
  where
    ex :: Expression -> Expression
    ex (Name nm b) = Name nm (ex b)
    ex (Pi n iT oT) = Pi n (ex iT) (ex oT)
    ex (Lambda n iT b) = Lambda n (ex iT) (ex b)
    ex (Apply f a) = Apply (ex f) (ex a)
    ex (Assert oT b) = Assert (ex oT) (ex b)
    ex (Hash n h) = maybe (Hash n h) (ex . entryExpr) $ HM.lookup h store
    ex x = x

