module Candid.Bool exposing (..)

import Candid exposing (..)

bool : Expr
bool =
  Pi "Bool" "r" Star <|
    Pi "" "true" (Ref 0) <|
      Pi "" "false" (Ref 1) <|
        Ref 2

true : Expr
true =
  Lam "True" "r" Star <|
    Lam "" "true" (Ref 0) <|
      Lam "" "false" (Ref 1) <|
        Ref 1

false : Expr
false =
  Lam "False" "r" Star <|
    Lam "" "true" (Ref 0) <|
      Lam "" "false" (Ref 1) <|
        Ref 0

and : Expr
and =
  Lam "and" "x" bool <|
    Lam "" "y" bool <|
      App (App (App (Ref 1) bool) (Ref 0)) false

or : Expr
or =
  Lam "and" "x" bool <|
    Lam "" "y" bool <|
      App (App (App (Ref 1) bool) true) (Ref 0)

