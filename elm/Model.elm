module Model exposing (Model)

import Candid exposing (..)

type alias Model =
  { expr : Expr
  , focus : Path
  }
