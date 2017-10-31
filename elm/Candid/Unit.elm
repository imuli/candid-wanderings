module Candid.Unit exposing (..)

import Candid exposing (..)

unit : Expr
unit =
  Pi "Unit" "r" Star <|
    Pi "" "" (Ref 0) <|
      (Ref 1)

id : Expr
id =
  Lam "id" "r" Star <|
    Lam "" "a" (Ref 0) <|
      (Ref 0)

