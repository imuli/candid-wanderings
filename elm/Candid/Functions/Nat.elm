module Candid.Functions.Nat exposing (..)

import Candid.Expr exposing (..)

nat : Expr
nat =
  Pi "Nat" "t" Star <|
    Pi "" "zero" (Ref 0) <|
      Pi "" "succ" (Pi "" "" (Ref 1) <| Ref 2) <|
        Ref 2

zero : Expr
zero =
  Lam "Zero" "t" Star <|
    Lam "" "zero" (Ref 0) <|
      Lam "" "succ" (Pi "" "" (Ref 1) <| Ref 2) <|
        Ref 1

succ : Expr
succ =
  Lam "Succ" "n" nat <|
    Lam "" "t" Star <|
      Lam "" "zero" (Ref 0) <|
        Lam "" "succ" (Pi "" "" (Ref 1) <| Ref 2) <|
          App "" (Ref 0) <| App "" (App "" (App "" (Ref 3) (Ref 2)) (Ref 1)) (Ref 0)

one : Expr
one = App "" succ zero

two : Expr
two = App "" succ one

add : Expr
add =
  Lam "+" "n" nat <|
    Lam "" "m" nat <|
      App "" (App "" (App "" (Ref 1) nat) (Ref 0)) succ

mul : Expr
mul =
  Lam "*" "n" nat <|
    Lam "" "m" nat <|
      App "" (App "" (App "" (Ref 1) nat) zero) (App "" add (Ref 0))

