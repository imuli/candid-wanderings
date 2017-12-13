module Subscription exposing (subscriptions)

import Model exposing (..)
import Message exposing (..)

import Keyboard

subscriptions : Model -> Sub Message
subscriptions model =
  Sub.batch
    [ Keyboard.presses KeyMsg
    ]

