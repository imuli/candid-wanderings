module Model exposing (Model)

import Candid.Expr exposing (..)

type alias Model =
  { expr : Expr
  , focus : Path
  }
