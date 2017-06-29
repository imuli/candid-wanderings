module Bool
  ( bool
  , true
  , false
  , Bool.or
  , Bool.and
  , Bool.not
  , Bool.xor
  ) where

import Core

bool :: Expr
bool = Pi (Const Star) $ Pi (Ref 0) $ Pi (Ref 1) $ Ref 2
true :: Expr
true = Lam (Const Star) $ Lam (Ref 0) $ Lam (Ref 1) $ Ref 1
false :: Expr
false = Lam (Const Star) $ Lam (Ref 0) $ Lam (Ref 1) $ Ref 0

or :: Expr
or = Lam bool $ Lam bool $ App (App (App (Ref 1) bool) true) (Ref 0)
or' :: Expr
or' = Lam bool $ App (App (Ref 0) bool) true

and :: Expr
and = Lam bool $ Lam bool $ App (App (App (Ref 1) bool) (Ref 0)) false

not :: Expr
not = Lam bool $ App (App (App (Ref 0) bool) false) true

xor :: Expr
xor = Lam bool $ Lam bool $ App (App (App (Ref 1) bool) (App Bool.not (Ref 0))) (Ref 0)

