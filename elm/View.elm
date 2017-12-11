module View exposing (view)

import Html exposing (Html, node, div, h2, span, text)
import Html.Attributes exposing (class, rel, href, style, attribute)
import Candid exposing (..)
import Blake2s1 exposing (..)
import Model exposing (..)
import Message exposing (..)

import Either exposing (..)
import List exposing (..)
import Tuple exposing (first, second)

view : Model -> Html Message
view model =
  div [class "expr"]
      [ node "link" [ rel "stylesheet", href "style.css" ] []
      , h2 [] [ text "Expression:" ]
      , viewExpr [] 0 model.expr
      , h2 [] [ text "Type:" ]
      , viewType (typeOf model.expr)
      , h2 [] [ text "Reduction:" ]
      , viewExpr [] 0 (reduce model.expr)
      ]

index : Int -> List a -> Maybe a
index i xs = head <| drop i xs

getRef : Int -> List (String, String) -> ((String, String) -> String) -> String -> String
getRef i reflist which default = case index i reflist of
                                      Nothing -> default
                                      Just p -> case which p of
                                                     "" -> default
                                                     a -> a

fixed : String -> Html Message
fixed t = span [ class "fixed" ] [ text t ]

viewStar : Html Message
viewStar = fixed "*"

viewHole : Html Message
viewHole = fixed "_"

viewExpr : List (String, String) -> Int -> Expr -> Html Message
viewExpr r i e =
  let is n = class ("r" ++ toString n)
      viewInner f = viewExpr r i f
      viewName f = span [class "name", is i] [text f]
      viewType a t = span [class "type"] [ span [class "typename"] [text a], viewInner t ]
      viewBody f a b = span [class "body"] [ viewExpr ((f,a) :: r) (i+1) b ]
      viewPiLam c f a t b = span [class c, is i] [viewName f, viewType a t, viewBody f a b ]
  in case e of
    Star        -> viewStar
    Hole        -> viewHole
    Pi  f a t b -> viewPiLam "pi right" f a t b
    Lam f a t b -> viewPiLam "lam right" f a t b
    App f a     -> span [class "app right"] [ span [class "func"] [viewInner f]
                                            , span [class "arg" ] [viewInner a]
                                            ]
    Ref n       -> span [class "ref", is (i-n-1), attribute "n" (toString n)] [ text <| getRef n r second (toString n) ]
    Rec n       -> span [class "rec", is (i-n-1), attribute "n" (toString n)] [ text <| getRef n r first (toString n) ]
    Note n f    -> span [class "note"] [ text "-- ", text n ]
    Type t b    -> span [class "type"] [ viewInner t, text "| ", viewInner b ]
    Hash h      -> span [class "hash"] [ text (toHex h) ]

viewTypeError : TypeError -> Html Message
viewTypeError te =
  span [] <| case te of
                  UnknownHash h      -> [ text "Unknown Hash: "
                                        , text (toHex h)
                                        ]
                  InvalidInputType p t -> [ text "Pi: "
                                          , viewExpr [] 0 p
                                          , text " has invalid input type: "
                                          , viewExpr [] 0 t
                                          ]
                  InvalidOutputType p t -> [ text "Pi: "
                                           , viewExpr [] 0 p
                                           , text " has invalid output type: "
                                           , viewExpr [] 0 t
                                          ]
                  NotAFunction f t -> [ text "Not a function:"
                                      , viewExpr [] 0 f
                                      , text "Actual Type:"
                                      , viewExpr [] 0 t
                                      ]
                  OpenExpression ctx x -> [ text "Open Expression:"
                                          , viewExpr [] 0 x
                                          ] ++ List.map (viewExpr [] 0) ctx
                  TypeMismatch app expect found -> [ div [] [ text "Type Mismatch at: "
                                                            , viewExpr [] 0 app
                                                            ]
                                                   , div [] [ text "expected type: "
                                                            , viewExpr [] 0 expect
                                                            ]
                                                   , div [] [ text "found type: "
                                                            , viewExpr [] 0 found
                                                            ]
                                                   ]

viewType : Either TypeError Expr -> Html Message
viewType e =
  case e of
       Left te -> viewTypeError te
       Right expr -> viewExpr [] 0 expr

