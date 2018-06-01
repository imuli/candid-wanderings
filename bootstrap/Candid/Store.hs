module Candid.Store
  ( Entry(..)
  , prettyEntry
  , Store(..)
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

data Entry = Entry { name :: String
                   , kind :: Expression Int
                   , expr :: Expression Int
                   , hash :: H.Hash
                   } deriving (Show)

prettyEntry :: Entry -> String
prettyEntry entry = "Name: " ++ name entry ++ "\nType: " ++ pretty [] (kind entry) ++ "\nExpression: " ++ pretty [] (expr entry) ++ "\nHash: " ++ H.toHex (Candid.Store.hash entry)

type Store = HM.HashMap H.Hash Entry

empty :: Store
empty = HM.empty

-- get all the entries with name
byName :: Store -> String -> [Entry]
byName store n = Prelude.map (\(k, v) -> v) $ HM.toList $ HM.filter ((n ==) . name) store

-- get the entry with hash
byHash :: Store -> H.Hash -> Maybe Entry
byHash store h = HM.lookup h store

-- add expression to store
add :: Store -> Expression Int -> Either TypeError (H.Hash, Store)
add store x = let x' = smush store x
                  t = typecheck (fmap expr . byHash store) (fmap kind . byHash store) False False [] x'
                  k = Candid.Expression.hash x'
               in t >>= \ty -> Right (k, HM.insert k (Entry (nameOf x) ty x' k) store)

-- replace sub-expressions that are in store with their hashes
smush :: Store -> Expression Int -> Expression Int
smush store = sm
  where
    lu expr = let h = Candid.Expression.hash expr
               in maybe expr (const $ Hash (nameOf expr) h) $ HM.lookup h store
    sm :: Expression Int -> Expression Int
    sm (Pi n bn iT oT) = lu $ Pi n bn (sm iT) (sm oT)
    sm (Lambda n bn iT b) = lu $ Lambda n bn (sm iT) (sm b)
    sm (Apply n f a) = lu $ Apply n (sm f) (sm a)
    sm (Assert n oT b) = lu $ Assert n (sm oT) (sm b)
    sm expr = expr

-- replace hashes that are in store with their content
expand :: Store -> Expression Int -> Expression Int
expand store = ex
  where
    ex :: Expression Int -> Expression Int
    ex (Pi n bn iT oT) = Pi n bn (ex iT) (ex oT)
    ex (Lambda n bn iT b) = Lambda n bn (ex iT) (ex b)
    ex (Apply n f a) = Apply n (ex f) (ex a)
    ex (Assert n oT b) = Assert n (ex oT) (ex b)
    ex (Hash n h) = maybe (Hash n h) (ex . expr) $ HM.lookup h store

