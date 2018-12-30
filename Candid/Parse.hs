{-#OPTIONS_GHC -Wall #-}
module Candid.Parse
  ( toExpression
  , loadExpr
  , loadExprs
  , exprP
  , parseText
  , splitExprs
  ) where

import Candid.Expression
import Candid.Store
import Text.Parsec hiding (spaces)
import Data.Maybe (listToMaybe, catMaybes)
import Data.List (elemIndex)

data ExprP
  = StarP Int
  | RefP String
  | TypeP ExprP ExprP
  | NameP String ExprP
  | PiP String ExprP ExprP
  | LamP String ExprP ExprP
  | AppP ExprP ExprP
  deriving (Show, Eq)

-- for converting to Expressions

find :: Store -> [String] -> String -> Maybe Expression
find store ctx str =
  case str of
       "" -> Nothing
       _ -> case elemIndex str ctx of
                 Just n -> Just $ Ref hole n
                 Nothing -> fmap hashFrom $ listToMaybe $ byName store str

toExpression :: Store -> ExprP -> Expression
toExpression store =
  let rec ctx expr =
        case expr of
             StarP n -> Star n
             RefP name -> maybe (Hole name) id $ find store ctx name
             NameP name body -> Name hole name (rec (name:ctx) body)
             PiP name inType outType -> Pi name (rec ctx inType) (rec (name:ctx) outType)
             LamP name inType body -> Lambda hole name (rec ctx inType) (rec (name:ctx) body)
             AppP function argument -> Apply hole (rec ctx function) (rec ctx argument)
             TypeP bodyType body -> rec ctx body `withType` rec ctx bodyType
   in rec []

-- for storing

loadExpr :: Store -> ExprP -> Either String Store
loadExpr store = fmap snd . add store . toExpression store

loadExprs :: Store -> [ExprP] -> Either String Store
loadExprs store =
  foldl (\st x -> either (const st) (flip loadExpr x) st) (Right store)

-- for parsing

{- The parser can be in a couple states:
 1. Nothing yet in this (sub)expression.
 2. Has a name.
 3. Has a label.
 4. Has an expression.
 4. Has an expression and a label.
-}

bichars :: String
bichars = ":=~⇒→"

nullchars :: String
nullchars = "*"

spaceChars :: String
spaceChars = " \t\r\n"

nameString :: Parsec String st String
nameString = many1 $ noneOf ("()" ++ spaceChars ++ nullchars ++ bichars)

spaces :: Parsec String st ()
spaces = () <$ many (oneOf " \t\r") <* optional (try (newline <* many1 (oneOf " \t")))

paren :: Parsec String st u -> Parsec String st u
paren inner = try $ char '(' *> inner <* spaces <* char ')'

starP :: Parsec String st ExprP
starP = StarP 0 <$ (char '*' <|> char '★') -- FIXME: parse subscripts

refP :: Parsec String st ExprP
refP = RefP <$> nameString

-- Nothing yet in this expression.
exprP :: Parsec String st ExprP
exprP = spaces *>
  choice [ starP >>= withExprP
         , paren exprP >>= withExprP
         , nameString >>= withNameP
         ]

-- With just a name, we need:
-- : starts a lambda or pi
-- = makes a name
-- anything else means this name is a reference
withNameP :: String -> Parsec String st ExprP
withNameP name = spaces *>
  choice [ char ':' *> withLabelP name
         , char '=' *> (NameP name <$> exprP)
         , withExprP $ RefP name
         ]

funcP :: Parsec String st ExprP
funcP = choice [starP, paren exprP, refP]

-- With an expression, we need:
-- → to make a pi
-- ⇒ to make a lambda
-- ~ to make a type assertion
-- another expression piece to make an apply
-- or we're done
withExprP :: ExprP -> Parsec String st ExprP
withExprP expr = spaces *>
  choice [ char '→' *> (PiP "" expr <$> exprP)
         , char '⇒' *> (LamP "" expr <$> exprP)
         , char '~' *> (TypeP expr <$> exprP)
         , (AppP expr <$> funcP) >>= withExprP
         , return expr <* spaces
         ]

inTypeP :: Parsec String st ExprP
inTypeP = choice [starP, paren exprP, refP]

-- With a label, we need an input type.
withLabelP :: String -> Parsec String st ExprP
withLabelP name = spaces *> inTypeP >>= withLabelExprP name

-- With a label and an expression, we need:
-- → to make a pi
-- ⇒ to make a lambda
-- another expression piece to make an apply
withLabelExprP :: String -> ExprP -> Parsec String st ExprP
withLabelExprP name expr = spaces *>
  choice [ char '→' *> (PiP name expr <$> exprP)
         , char '⇒' *> (LamP name expr <$> exprP)
         , (AppP expr <$> funcP) >>= withLabelExprP name
         ]

exprsP :: Parsec String st [ExprP]
exprsP = fmap catMaybes $ optionMaybe exprP `sepBy` char '\n'

parseText :: String -> Either ParseError [ExprP]
parseText = parse exprsP ""

-- various parsing things
splitExprs :: String -> [String]
splitExprs source =
  let with :: a -> [[a]] -> [[a]]
      with x xs = (x : head xs) : tail xs
   in case source of
           "" -> [""]
           '\n' : ' ' : xs -> with '\n' $ with ' ' $ splitExprs xs
           '\n' : '\t' : xs -> with '\t' $ with ' ' $ splitExprs xs
           '\n' : xs -> "" : splitExprs xs
           x : xs -> with x $ splitExprs xs
