module View exposing (view)

import Html exposing (Html)

import Style
import Style.Font
import Style.Color
import Element exposing (el, row, column, text)
import Element.Attributes exposing (center, inlineStyle, padding, spacing)

import Candid exposing (..)
import Blake2s1 exposing (..)
import Model exposing (..)
import Message exposing (..)

import Either exposing (..)
import List exposing (..)
import Tuple exposing (first, second)

type Styles
  = Title
  | Header
  | Main
  | Plain
  | TypeRow
  | ExprRow

stylesheet = Style.styleSheet
  [ Style.style Title [ Style.Font.size 24, Style.Font.center ]
  , Style.style Main [ Style.Font.size 16 ]
  , Style.style Header [ Style.Font.size 18 ]
  , Style.style Plain [ ]
  , Style.style TypeRow [ ]
  , Style.style ExprRow [ ]
  ]

view : Model -> Html Message
view model = Element.layout stylesheet <|
  column Main [ padding 10, spacing 10]
  [ el Title [center] (Element.text "candid")
  , row TypeRow [] [ el Header [] (text "Type: "), viewType (typecheck model.expr [] False) ]
  , row ExprRow [] [ viewExpr model.focus [] model.expr [] 0 ]
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
    Right expr -> "#" ++ (String.left 6 <| toHex <| Candid.hash <| muddle (List.length ctx) expr)

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

viewExpr : Path -> Path -> Expr -> List Expr -> Int -> Element.Element Styles variation Message
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
        then row Plain [] [ text "(", x, text ")" ]
        else x
      -- wrap in a color
      wrapColor eExpr = el Plain [(inlineStyle [ colorStyle eExpr ])]
      -- wrapper for expressions
      wrapTypeColor = wrapColor (typecheck expr ctx False)
      -- helper for expression names
      viewName n = string n [] <| always [ wrapColor (typecheck expr ctx False) (text n)
                                         , text " = " ]
      -- helper for argument names
      viewArgname ty n = string n [] <| always [ wrapColor (Right ty) (text n)
                                               , text " : " ]
  in par <| case expr of
        Star        -> wrapTypeColor <| text "★"
        Hole        -> wrapTypeColor <| text "_"
        Ref i       -> wrapTypeColor <| text <| string (argname (index i ctx)) ("!" ++ toString i) identity
        Rec i       -> wrapTypeColor <| text <| string (name (index i ctx)) ("@" ++ toString i) identity
        Hash n h    -> wrapTypeColor <| text <| string n (toUni h) identity
        Note s b    -> row Plain []
                       [ text ("-- " ++ s)
                       , viewSub Rightward b ctx paren
                       ]
        App n f a   -> row Plain [] <|
                       viewName n ++
                       [ viewSub Leftward f ctx 1
                       , text " "
                       , viewSub Rightward a ctx 2
                       ]
        Pi n a t b  -> row Plain [] <|
                       viewName n ++ viewArgname t a ++
                       [ viewSub Leftward t ctx 1
                       , text " ⇒ "
                       , viewSub Rightward b (expr :: ctx) 0
                       ]
        Type n t b  -> column Plain []
                       [ row Plain [] <| viewName n ++ [ viewSub Leftward t ctx 1 ]
                       , viewSub Rightward b ctx 0
                       ]
        Lam n a t b -> row Plain [] <|
                       viewName n ++ viewArgname t a ++
                       [ viewSub Leftward t ctx 1
                       , text " → "
                       , viewSub Rightward b (expr :: ctx) 0
                       ]

viewIn : Expr -> List Expr -> Element.Element Styles variation Message
viewIn expr ctx = viewExpr [] [None] expr ctx 0

viewTypeError : TypeError -> Element.Element Styles variation Message
viewTypeError te = case te of
    BadContext -> text "⸘Bad Context‽"
    UnknownHash h -> row Plain []
      [ text "Unknown Hash: "
      , text (toHex h)
      ]
    OpenExpression ctx x -> row Plain []
      [ text "Open Expression: "
      , viewIn x ctx
      ]
    TypeInference ctx expr -> row Plain []
      [ text "Type Inference at: "
      , viewIn expr ctx
      ]
    TypeMismatch ctx app expected actual -> column Plain []
      [ row Plain [] [ text "Type Mismatch at: ", viewIn app ctx ]
      , row Plain [] [ text "Expected type: ", viewIn expected ctx ]
      , row Plain [] [ text "Actual type: ", viewIn actual ctx ]
      ]

viewType : Either TypeError Expr -> Element.Element Styles variation Message
viewType e =
  case e of
       Left te -> viewTypeError te
       Right expr -> viewIn expr []

