module Update exposing (update)
import Model exposing (..)
import Message exposing (..)

import Candid exposing (Replacement(..), Step(..), Expr(..), lookup)

import List exposing (reverse, drop)
import Char exposing (fromCode)

noop : Model -> ( Model, Cmd Message )
noop model = ( model, Cmd.none )

up : Model -> ( Model, Cmd Message )
up model = ( { model | focus = drop 1 model.focus }, Cmd.none )

name : Expr -> Model -> ( Model, Cmd Message )
name expr model =
  let op = ( { model | focus = Name :: model.focus }, Cmd.none ) in
      case expr of
        Lam _ _ _ _ -> op
        Pi _ _ _ _ -> op
        Type _ _ _ -> op
        App _ _ _ -> op
        Hash _ _ -> op
        _ -> noop model

update : Message -> Model -> ( Model, Cmd Message )
update msg model =
  case msg of
    NoOp -> ( model, Cmd.none )
    KeyMsg code ->
      case lookup model.expr (reverse model.focus) of
        -- string edit mode
        Nothing -> up model
        Just (S s) -> case fromCode code of
          ' ' -> up model
          _ -> ( model, Cmd.none )
        Just (N i) -> case fromCode code of
          'k' -> up model
          _ -> ( model, Cmd.none )
        Just (E expr) -> case fromCode code of
          'k' -> up model
          'n' -> name expr model
          _ -> ( model, Cmd.none )

