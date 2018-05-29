module Candid.Store
  ( Entry(..)
  , Store(..)
  , empty
  , byName
  , add
  , smush
  , expand
  ) where

import Candid.Expression
import qualified Data.HashMap.Strict as HM
import qualified Blake2s1 as H

data Entry = Entry { name :: String
                   , kind :: Expression Word
                   , expr :: Expression Word
                   , hash :: H.Hash
                   } deriving (Show)

type Store = HM.HashMap H.Hash Entry

empty :: Store
empty = HM.empty

-- get all the entries with name
byName :: Store -> String -> [Entry]
byName store n = Prelude.map (\(k, v) -> v) $ HM.toList $ HM.filter ((n ==) . name) store

-- add expression to store
add :: Store -> Expression Word -> (Maybe H.Hash, Store)
add store expr = let c = closed expr
                     k = Candid.Expression.hash expr
                     expr' = smush store expr
                     entry = (Entry (nameOf expr) (Hole "") expr' k)
                  in if not c
                        then (Nothing, store)
                        else (Just k, HM.insert k entry store)

-- replace sub-expressions that are in store with their hashes
smush :: Store -> Expression Word -> Expression Word
smush store = sm
  where
    lu expr = let h = Candid.Expression.hash expr
               in maybe expr (const $ Hash h) $ HM.lookup h store
    sm :: Expression Word -> Expression Word
    sm (Pi n bn iT oT) = lu $ Pi n bn (sm iT) (sm oT)
    sm (Lambda n bn iT b) = lu $ Lambda n bn (sm iT) (sm b)
    sm (Apply n f a) = lu $ Apply n (sm f) (sm a)
    sm (Assert n oT b) = lu $ Assert n (sm oT) (sm b)
    sm expr = expr

-- replace hashes that are in store with their content
expand :: Store -> Expression Word -> Expression Word
expand store = ex
  where
    ex :: Expression Word -> Expression Word
    ex (Pi n bn iT oT) = Pi n bn (ex iT) (ex oT)
    ex (Lambda n bn iT b) = Lambda n bn (ex iT) (ex b)
    ex (Apply n f a) = Apply n (ex f) (ex a)
    ex (Assert n oT b) = Assert n (ex oT) (ex b)
    ex (Hash h) = maybe (Hash h) (ex . expr) $ HM.lookup h store

