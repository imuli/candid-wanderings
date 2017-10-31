module Update exposing (update)
import Model exposing (..)
import Message exposing (..)

update : Message -> Model -> ( Model, Cmd Message )
update msg model =
  case msg of
    NoOp -> ( model, Cmd.none )

