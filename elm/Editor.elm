module Editor exposing (..)

import Html exposing (Html, program)

import Candid.Expr exposing (..)

import Model exposing (..)
import Message exposing (..)
import View exposing (..)
import Update exposing (..)
import Subscription exposing (..)

import Candid.Functions.Bool as Bool
import Candid.Functions.List as List
import Candid.Functions.Nat as Nat
import Candid.Functions.Natural as Natural
import Candid.Functions.Pair as Pair
import Candid.Functions.Unit as Unit

init : (Model, Cmd Message)
init = ( { expr = App "" (App "" Bool.xor Bool.false) Bool.true
         , focus = []
         }
       , Cmd.none )

main : Program Never Model Message
main =
  program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

