module View exposing (view)

import Html exposing (Html, node, div, h3, span, br, text)
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
  div [ style [ ("font-size","1.5em"), ("padding", "1ex") ] ]
      [ h3 [ style [ ("display","inline-block"), ("padding-right","1em") ] ] [ text "Type: " ]
      , viewType (typecheck model.expr [] False)
      , br [] []
      , viewExpr model.focus [] model.expr [] 0
      ]

index : Int -> List a -> Maybe a
index i xs = head <| drop i xs

kind : Expr -> String
kind expr = case expr of
  Star        -> "star"
  Hole        -> "hole"
  Ref _       -> "ref"
  Rec _       -> "rec"
  Hash _ _    -> "hash"
  Note _ b    -> "note"
  App _ f a   -> "app"
  Pi _ _ t b  -> "pi"
  Type _ t b  -> "type"
  Lam _ _ t b -> "lam"

name : Maybe Expr -> String
name expr = case expr of
  Just (Hash n _)    -> n
  Just (App n f a)   -> n
  Just (Pi n _ t b)  -> n
  Just (Type n t b)  -> n
  Just (Lam n _ t b) -> n
  _ -> ""

argname : Maybe Expr -> String
argname expr = case expr of
  Just (Pi _ a t b)  -> a
  Just (Lam _ a t b) -> a
  _ -> ""

string : String -> a -> (String -> a) -> a
string s empty full =
  case s of
    "" -> empty
    _  -> full s

{- rewrite references in open expressions to count down rather than up -}
muddle : Int -> Expr -> Expr
muddle depth expr =
  if closed expr < 0
  then expr
  else case expr of
    Star -> expr
    Hole -> expr
    Hash _ _ -> expr
    Ref v -> Ref (depth - v)
    Rec v -> Rec (depth - v)
    Type name ty body -> Type name (muddle depth ty) (muddle depth body)
    App name func arg -> App name (muddle depth func) (muddle depth arg)
    Pi name argname ty body -> Pi name argname (muddle depth ty) (muddle (depth+1) body)
    Lam name argname ty body -> Lam name argname (muddle depth ty) (muddle (depth+1) body)
    Note str body -> Note str (muddle depth body)

colorExpr : Either a Expr -> List Expr -> String
colorExpr eExpr ctx =
  case eExpr of
    Left _ -> "black"
    Right expr -> "/* " ++ toString (muddle (List.length ctx) expr) ++ " */ " ++ "#" ++ (String.left 6 <| toHex <| Candid.hash <| muddle (List.length ctx) expr)

parExpr : Expr -> Int
parExpr expr = case expr of
        Star        -> 99
        Hole        -> 99
        Ref i       -> 99
        Rec i       -> 99
        Hash n h    -> 99
        Note s b    -> 99
        App n f a   -> 2
        Pi n a t b  -> 1
        Type n t b  -> 1
        Lam n a t b -> 1

viewExpr : Path -> Path -> Expr -> List Expr -> Int -> Html Message
viewExpr focus path expr context paren =
  let -- discard extra context
      ctx = if closed expr < 0 then [] else context
      -- helper to view a subexpression
      viewSub step = viewExpr focus (step :: path)
      -- when this expression has focus
      focusStyle = if focus == path then ("outline", "1px solid #80ff80") else ("","")
      -- color things by their type
      colorStyle eExpr = ("color", colorExpr eExpr ctx)
      -- wrap expression in parens
      par x =
        if paren >= parExpr expr
        then span [ class "candid-paren" ] [ text "(", x, text ")" ]
        else x
      -- wrap in a color
      wrapColor eExpr = span [ style [ colorStyle eExpr ] ]
      -- wrapper for expressions
      view xs = span [ class ("candid-" ++ kind expr)
                     , style <| focusStyle :: if List.length xs == 1 then [ colorStyle (typecheck expr ctx False) ] else []
                     ] xs
      -- helper for expression names
      viewName n = string n [] <| always [ wrapColor (typecheck expr ctx False) [ text n ]
                                         , text " = " ]
      -- helper for argument names
      viewArgname ty n = string n [] <| always [ wrapColor (Right ty) [ text n ]
                                               , text " : " ]
  in par <| view <| case expr of
        Star        -> [ text "★" ]
        Hole        -> [ text "_" ]
        Ref i       -> [ text <| string (argname (index i ctx)) ("!" ++ toString i) identity ]
        Rec i       -> [ text <| string (name (index i ctx)) ("@" ++ toString i) identity ]
        Hash n h    -> [ text <| string n (toUni h) identity ]
        Note s b    -> [ text ("-- " ++ s)
                       , viewSub Rightward b ctx paren
                       ]
        App n f a   -> viewName n ++
                       [ viewSub Leftward f ctx 1
                       , text " "
                       , viewSub Rightward a ctx 2
                       ]
        Pi n a t b  -> viewName n ++ viewArgname t a ++
                       [ viewSub Leftward t ctx 1
                       , text " ⇒ "
                       , viewSub Rightward b (expr :: ctx) 0
                       ]
        Type n t b  -> viewName n ++
                       [ viewSub Leftward t ctx 1
                       , br [] []
                       , viewSub Rightward b ctx 0
                       ]
        Lam n a t b -> viewName n ++ viewArgname t a ++
                       [ viewSub Leftward t ctx 1
                       , text " → "
                       , viewSub Rightward b (expr :: ctx) 0
                       ]

viewIn : Expr -> List Expr -> Html Message
viewIn expr ctx = viewExpr [] [None] expr ctx 0

viewTypeError : TypeError -> Html Message
viewTypeError te =
  let xpect ctx expected actual =
        [ text "Expected type: "
        , viewIn expected ctx
        , br [] []
        , text "Actual type: "
        , viewIn actual ctx
        ]
  in span [] <| case te of
    BadContext -> [ text "⸘Bad Context‽" ]
    UnknownHash h ->
      [ text "Unknown Hash: "
      , text (toHex h)
      ]
    OpenExpression ctx x ->
      [ text "Open Expression:"
      , viewIn x ctx
      ]
    TypeInference ctx expr ->
      [ text "Type Inference at: "
      , viewIn expr ctx
      ]
    TypeMismatch ctx app expected actual ->
      [ text "Type Mismatch at: "
      , viewIn app ctx
      , br [] []
      ] ++ xpect ctx expected actual

viewType : Either TypeError Expr -> Html Message
viewType e =
  case e of
       Left te -> viewTypeError te
       Right expr -> viewIn expr []

