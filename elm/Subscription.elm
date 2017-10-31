module Subscription exposing (subscriptions)
import Model exposing (..)
import Message exposing (..)

subscriptions : Model -> Sub Message
subscriptions model =
  Sub.none

