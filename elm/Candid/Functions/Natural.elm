module Candid.Functions.Natural exposing (..)

import Candid.Expr exposing (..)

natural : Expr
natural =
  Pi "Natural" "t" Star <|
    Pi "" "zero" (Ref 0) <|
      Pi "" "succ" (Pi "" "" (Rec 1) <| Ref 2) <|
        Ref 2

zero : Expr
zero =
  Lam "Zero" "t" Star <|
    Lam "" "zero" (Ref 0) <|
      Lam "" "succ" (Pi "" "" natural <| Ref 2) <|
        Ref 1

succ : Expr
succ =
  Lam "Succ" "n" natural <|
    Lam "" "t" Star <|
      Lam "" "zero" (Ref 0) <|
        Lam "" "succ" (Pi "" "" natural <| Ref 2) <|
          App "" (Ref 0) (Ref 3)

fold : Expr
fold =
  Lam "fold" "t" Star <|
    Lam "" "f" (Pi "" "" (Ref 0) (Ref 1)) <|
      Lam "" "a" (Ref 1) <|
        Lam "fold'" "n" natural <|
          Type "" (Ref 3) <|
            App "" (App "" (App "" (Ref 0) (Ref 3)) (Ref 1)) <|
              Lam "" "n-1" natural <|
                App "" (Ref 3) <| App "" (Rec 1) (Ref 0)

prec : Expr
prec =
  Lam "prec" "n" natural <|
    App "" (App "" (App "" (Ref 0) natural) zero) (Lam "" "p" natural (Ref 0))

add : Expr
add =
  Lam "+" "n" natural <|
    Lam "" "m" natural <|
      Type "" natural <|
        App "" (App "" (App "" (Ref 0) natural) (Ref 1)) (App "" (Rec 1) (App "" succ (Ref 1)))

add_ : Expr
add_ =
  Lam "+" "n" natural <|
    Lam "" "m" natural <|
      Type "" natural <|
        App "" (App "" (App "" (Ref 1) natural) (Ref 0)) <|
          Lam "" "n-1" natural <|
            App "" (App "" (Rec 2) (Ref 0)) (App "" succ (Ref 1))

addf : Expr
addf =
  Lam "+" "n" natural <|
    Lam "" "m" natural <|
      App "" (App "" (App "" (App "" fold natural) succ) (Ref 0)) (Ref 1)

mul : Expr
mul =
  Lam "*" "n" natural <|
    Lam "" "m" natural <|
      App "" (App "" (App "" (App "" fold natural) (App "" add (Ref 0))) zero) (Ref 1)

one : Expr
one = App "" succ zero

exp : Expr
exp =
  Lam "^" "n" natural <|
    Lam "" "m" natural <|
      App "" (App "" (App "" (App "" fold natural) (App "" mul (Ref 1))) one) (Ref 0)

two : Expr
two = App "" succ one

four : Expr
four = reduce <| App "" (App "" add two) two

eight : Expr
eight = reduce <| App "" (App "" add four) four

