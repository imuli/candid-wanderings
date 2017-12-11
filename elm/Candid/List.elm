module Candid.List exposing (..)

import Candid exposing (..)

list : Expr
list =
  Lam "List" "t" Star <|
    Pi "" "r" Star <|
      Pi "" "nil" (Ref 0) <|
        Pi "" "cons" (Pi "" "" (Ref 2) <| Pi "" "" (Ref 2) <| Ref 3) <|
          Ref 2

nil : Expr
nil =
  Lam "Nil" "t" Star <|
    Lam "" "r" Star <|
      Lam "" "nil" (Ref 0) <|
        Lam "" "cons" (Pi "" "" (Ref 2) <| Pi "" "" (Ref 2) <| Ref 3) <|
          Ref 1

cons : Expr
cons =
  Lam "Cons" "t" Star <|
    Lam "" "a" (Ref 0) <|
      Lam "" "rest" (App "" list (Ref 0)) <|
        Lam "" "r" Star <|
          Lam "" "nil" (Ref 0) <|
            Lam "" "cons" (Pi "" "" (Ref 2) <| Pi "" "" (Ref 2) <| Ref 3) <|
              App "" (App "" (Ref 0) (Ref 4)) (App "" (App "" (App "" (Ref 3) (Ref 2)) (Ref 1)) (Ref 0))

