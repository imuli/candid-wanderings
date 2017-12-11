module Editor exposing (..)

import Html exposing (Html, program)

import Candid exposing (..)

import Model exposing (..)
import Message exposing (..)
import View exposing (..)
import Update exposing (..)
import Subscription exposing (..)

import Candid.Unit as U
import Candid.Bool as B
import Candid.Pair as P
import Candid.NatR as NR
import Candid.Nat as N
import Candid.List as L

init : (Model, Cmd Message)
init = ( { expr = App "" NR.exp NR.one }
       , Cmd.none )

main : Program Never Model Message
main =
  program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

