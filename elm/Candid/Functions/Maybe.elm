module Candid.Functions.Maybe exposing (..)

import Candid.Expr exposing (..)

maybe : Expr
maybe =
  Lam "Maybe" "t" Star <|
    Pi "" "r" Star <|
      Pi "" "nothing" (Ref 0) <|
        Pi "" "just" (Pi "" "this" (Ref 2) (Ref 2)) <|
          Ref 2

nothing : Expr
nothing =
  Lam "Nothing" "t" Star <|
    Lam "" "r" Star <|
      Lam "" "nothing" (Ref 0) <|
        Lam "" "just" (Pi "" "this" (Ref 2) (Ref 2)) <|
          Ref 1

just : Expr
just =
  Lam "Just" "t" Star <|
    Lam "" "this" (Ref 0) <|
      Lam "" "r" Star <|
        Lam "" "nothing" (Ref 0) <|
          Lam "" "just" (Pi "" "this" (Ref 3) (Ref 2)) <|
            App "" (Ref 0) (Ref 3)

map : Expr
map =
  Lam "map" "t" Star <|
    Lam "" "r" Star <|
      Lam "" "fn" (Pi "" "" (Ref 1) (Ref 1)) <|
        Lam "" "mx" (App "" maybe (Ref 2)) <|
          App "" (App "" (App "" (Ref 0) (App "" maybe (Ref 2))) (App "" nothing (Ref 2))) <|
            Lam "" "x" (Ref 3) <|
              App "" (App "" just (Ref 3)) (App "" (Ref 2) (Ref 0))
