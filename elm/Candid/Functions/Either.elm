module Candid.Functions.Either exposing (..)

import Candid.Expr exposing (..)

either : Expr
either =
  Lam "Either" "t" Star <|
    Lam "" "s" Star <|
      Pi "" "r" Star <|
        Pi "" "left" (Pi "" "a" (Ref 2) (Ref 1)) <|
          Pi "" "right" (Pi "" "b" (Ref 2) (Ref 2)) <|
            Ref 2

left : Expr
left =
  Lam "Left" "t" Star <|
    Lam "" "s" Star <|
      Lam "" "a" (Ref 1) <|
        Lam "" "r" Star <|
          Lam "" "left" (Pi "" "a" (Ref 3) (Ref 1)) <|
            Lam "" "right" (Pi "" "b" (Ref 3) (Ref 2)) <|
              App "" (Ref 1) (Ref 3)

right : Expr
right =
  Lam "Right" "t" Star <|
    Lam "" "s" Star <|
      Lam "" "b" (Ref 0) <|
        Lam "" "r" Star <|
          Lam "" "left" (Pi "" "a" (Ref 3) (Ref 1)) <|
            Lam "" "right" (Pi "" "b" (Ref 3) (Ref 2)) <|
              App "" (Ref 0) (Ref 3)

map : Expr
map =
  Lam "map" "t" Star <|
    Lam "" "s" Star <|
      Lam "" "r" Star <|
        Lam "" "fn" (Pi "" "" (Ref 1) (Ref 1)) <|
          Lam "" "either" (App "" (App "" either (Ref 3)) (Ref 2)) <|
            App "" (App "" (App "" (Ref 0) (App "" (App "" either (Ref 4)) (Ref 2))) (App "" (App "" left (Ref 4)) (Ref 2))) <|
              Lam "" "b" (Ref 3) <|
                App "" (App "" (App "" right (Ref 5)) (Ref 3)) (App "" (Ref 2) (Ref 0))

mapLeft : Expr
mapLeft =
  Lam "map" "t" Star <|
    Lam "" "s" Star <|
      Lam "" "r" Star <|
        Lam "" "fn" (Pi "" "" (Ref 2) (Ref 1)) <|
          Lam "" "either" (App "" (App "" either (Ref 3)) (Ref 2)) <|
            App "" (App "" (App "" (Ref 0) (App "" (App "" either (Ref 2)) (Ref 3)))
              (Lam "" "a" (Ref 4) <|
                App "" (App "" (App "" left (Ref 3)) (Ref 4)) (App "" (Ref 2) (Ref 0)))) <|
                  App "" (App "" right (Ref 2)) (Ref 3)
