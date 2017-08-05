{-#OPTIONS_GHC -Wall #-}

module Candid.Store
  ( Store
  , empty
  , find
  , add
  , unhash
  , hashOf
  , hashWith
  , hashInto
  ) where

import Candid.Expr
import Candid.Hash
import Data.Map as M

type Store = Map Hash (Expr, Expr)

hashOf :: Expr -> Expr
hashOf e = Hash $ hash e

find :: Expr -> Store -> Maybe (Expr, Expr)
find e st = M.lookup (hash e) st

add :: Expr -> Expr -> Store -> Store
add e t st = M.insertWith (flip const) (hash e) (e, t) st

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

hashInto :: Expr -> Expr -> Store -> Store
hashInto e t st = add (hashWith st e) (hashWith st t) st

unhash :: Store -> Expr -> Expr
unhash st = uh
  where
    uh e = case e of
                App f a -> App (uh f) (uh a)
                Lam t f -> Lam (uh t) (uh f)
                Pi t f  -> Pi (uh t) (uh f)
                Rem s x -> Rem s (uh x)
                Hash h  -> maybe e (uh . fst) $ M.lookup h st
                TA t f -> TA (uh t) (uh f)
                _       -> e

