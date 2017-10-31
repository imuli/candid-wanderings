module Candid.NatR exposing (..)

import Candid exposing (..)

nat : Expr
nat =
  Pi "Nat" "t" Star <|
    Pi "" "zero" (Ref 0) <|
      Pi "" "succ" (Pi "" "" (Rec 1) <| Ref 2) <|
        Ref 2

zero : Expr
zero =
  Lam "Zero" "t" Star <|
    Lam "" "zero" (Ref 0) <|
      Lam "" "succ" (Pi "" "" nat <| Ref 2) <|
        Ref 1

succ : Expr
succ =
  Lam "Succ" "n" nat <|
    Lam "" "t" Star <|
      Lam "" "zero" (Ref 0) <|
        Lam "" "succ" (Pi "" "" nat <| Ref 2) <|
          App (Ref 0) (Ref 3)

fold : Expr
fold =
  Lam "fold" "t" Star <|
    Lam "" "f" (Pi "" "" (Ref 0) (Ref 1)) <|
      Lam "" "a" (Ref 1) <|
        Lam "fold'" "n" nat <|
          Type (Ref 3) <|
            App (App (App (Ref 0) (Ref 3)) (Ref 1)) <|
              Lam "" "n-1" nat <|
                App (Ref 3) <| App (Rec 1) (Ref 0)

prec : Expr
prec =
  Lam "prec" "n" nat <|
    App (App (App (Ref 0) nat) zero) (Lam "" "p" nat (Ref 0))

add : Expr
add =
  Lam "+" "n" nat <|
    Lam "" "m" nat <|
      Type nat <|
        App (App (App (Ref 0) nat) (Ref 1)) (App (Rec 1) (App succ (Ref 1)))

add_ : Expr
add_ =
  Lam "+" "n" nat <|
    Lam "" "m" nat <|
      Type nat <|
        App (App (App (Ref 1) nat) (Ref 0)) <|
          Lam "" "n-1" nat <|
            App (App (Rec 2) (Ref 0)) (App succ (Ref 1))

addf : Expr
addf =
  Lam "+" "n" nat <|
    Lam "" "m" nat <|
      App (App (App (App fold nat) succ) (Ref 0)) (Ref 1)

mul : Expr
mul =
  Lam "*" "n" nat <|
    Lam "" "m" nat <|
      App (App (App (App fold nat) (App add (Ref 0))) zero) (Ref 1)

one : Expr
one = App succ zero

exp : Expr
exp =
  Lam "^" "n" nat <|
    Lam "" "m" nat <|
      App (App (App (App fold nat) (App mul (Ref 1))) one) (Ref 0)

two : Expr
two = App succ one

four : Expr
four = reduce <| App (App add two) two

eight : Expr
eight = reduce <| App (App add four) four

