{-#OPTIONS_GHC -Wall #-}

module Candid.Util
  ( replace
  , shift
  ) where

import Candid.Expr

-- general form used by the following helpers
rec :: (Word -> Word -> Expr) -> Word -> Expr -> Expr
rec ref = i
  where
    i c e = case e of
                 Ref n   -> ref n c
                 App f a -> App (i c f) (i c a)
                 Lam t f -> Lam (i c t) (i (c+1) f)
                 Pi t f  -> Pi (i c t) (i (c+1) f)
                 Rem s x -> Rem s $ i c x
                 TA t x  -> TA t $ i c x
                 _ -> e

-- adjust instances of (Ref 0) and above by some amount
shift :: Word -> Expr -> Expr
shift z = rec (\n c -> Ref $ if n >= c then (n + z) else n) 0

-- replace instances of (Ref 0)
replace :: Expr -> Expr -> Expr
replace a = shift (-1) . rec (\n c -> if n == c then shift (c+1) a else Ref n) 0
