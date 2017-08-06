{-#OPTIONS_GHC -Wall #-}

module Candid.Store
  ( Store
  , Entry
  , entryExpr
  , entryType
  , entryName
  , empty
  , find
  , findName
  , add
  , unhash
  , hashOf
  , hashWith
  , hashInto
  ) where

import Candid.Expr
import Candid.Hash
import Data.Map as M

type Entry = (String, Expr, Expr)
type Store = Map Hash Entry

entryName :: Entry -> String
entryName (n, _, _) = n

entryExpr :: Entry -> Expr
entryExpr (_, e, _) = e

entryType :: Entry -> Expr
entryType (_, _, t) = t

hashOf :: Expr -> Expr
hashOf e = Hash $ hash e

find :: Expr -> Store -> Maybe Entry
find e st = M.lookup (hash e) st

findName :: String -> Store -> Maybe Entry
findName n st = M.lookup (hash n) st

add :: String -> Expr -> Expr -> Store -> Store
add n e t st = M.insert (hash n) (n, e,t) $ M.insertWith (flip const) (hash e) (n, e, t) st

hashWith :: Store -> Expr -> Expr
hashWith st = hw
  where
    ify :: Expr -> Expr
    ify x = let h = hash x
             in maybe x (const $ Hash h) $ M.lookup h st
    hw :: Expr -> Expr
    hw e = case e of
              App f a -> ify $ App (hw f) (hw a)
              Lam t f -> ify $ Lam (hw t) (hw f)
              Pi t f  -> ify $ Pi (hw t) (hw f)
              Rem s x -> Rem s $ hw x
              TA t f  -> TA (hw t) (hw f)
              _ -> e

hashInto :: String -> Expr -> Expr -> Store -> Store
hashInto n e t st = add n (hashWith st e) (hashWith st t) st

unhash :: Store -> Expr -> Expr
unhash st = uh
  where
    uh e = case e of
                App f a -> App (uh f) (uh a)
                Lam t f -> Lam (uh t) (uh f)
                Pi t f  -> Pi (uh t) (uh f)
                Rem s x -> Rem s (uh x)
                Hash h  -> maybe e (uh . entryExpr) $ M.lookup h st
                TA t f -> TA (uh t) (uh f)
                _       -> e

