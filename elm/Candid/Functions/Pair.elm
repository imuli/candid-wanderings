module Candid.Functions.Pair exposing (..)

import Candid.Expr exposing (..)

pair : Expr
pair =
  Lam "Pair" "t" Star <|
    Lam "" "s" Star <|
      Pi "" "r" Star <|
        Pi "" "f" (Pi "" "a" (Ref 2) <| Pi "" "b" (Ref 2) <| Ref 2) <|
          Ref 1

p : Expr
p =
  Lam "P" "t" Star <|
    Lam "" "a" (Ref 0) <|
      Lam "" "s" Star <|
        Lam "" "b" (Ref 0) <|
          Lam "" "r" Star <|
            Lam "" "f" (Pi "" "" (Ref 4) <| Pi "" "" (Ref 3) <| Ref 2) <|
              App "" (App "" (Ref 0) (Ref 4)) (Ref 2)

