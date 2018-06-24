{-#OPTIONS_GHC -Wall #-}
module Candid.Parse where

import Candid.Expression
import Candid.Store
import Text.Parsec hiding (spaces)
import Data.Maybe (listToMaybe, catMaybes)
import Data.List (elemIndex)

data ExprP
  = StarP
  | RefP String
  | TypeP ExprP ExprP
  | NameP String ExprP
  | PiP String ExprP ExprP
  | LamP String ExprP ExprP
  | AppP [ExprP]
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
             StarP -> Star
             RefP name -> maybe (Hole name) id $ find store ctx name
             NameP name body -> Name hole name (rec (name:ctx) body)
             PiP name inType outType -> Pi name (rec ctx inType) (rec (name:ctx) outType)
             LamP name inType body -> Lambda hole name (rec ctx inType) (rec (name:ctx) body)
             AppP [] -> hole
             AppP (x:xs) -> foldl (Apply hole) (rec ctx x) $ map (rec ctx) xs
             TypeP bodyType body -> rec ctx body `withType` rec ctx bodyType
   in rec []

-- for storing

loadExpr :: Store -> ExprP -> Either String Store
loadExpr store = fmap snd . add store . toExpression store

loadExprs :: Store -> [ExprP] -> Either String Store
loadExprs store =
  foldl (\st x -> either (const st) (flip loadExpr x) st) (Right store)

-- for parsing

bichars :: String
bichars = ":=~⇒→"

nullchars :: String
nullchars = "*"

spaceChars :: String
spaceChars = " \t\r\n"

nameString :: Parsec String st String
nameString = many1 $ noneOf ("()" ++ spaceChars ++ nullchars ++ bichars)

maybeName :: Parsec String st String
maybeName = try (nameString <* spaces <* char ':') <|> string ""

spaces1 :: Parsec String st ()
spaces1 = () <$ optional (try (many (oneOf " \t\r") <* newline)) <* many1 (oneOf " \t")

spaces :: Parsec String st ()
spaces = () <$ many (oneOf " \t\r") <* optional (try (newline <* many1 (oneOf " \t")))

paren :: Parsec String st u -> Parsec String st u
paren inner = try $ char '(' *> inner <* spaces <* char ')'

starP :: Parsec String st ExprP
starP = StarP <$ char '*'

refP :: Parsec String st ExprP
refP = RefP <$> nameString

typeP :: Parsec String st ExprP
typeP = try $ TypeP <$> inTypeP <* char '~' <*> exprP

nameP :: Parsec String st ExprP
nameP = try $ NameP <$> nameString <* spaces <* char '=' <*> exprP

piP :: Parsec String st ExprP
piP = try $ PiP <$> maybeName <*> inTypeP <* char '→' <*> exprP

lamP :: Parsec String st ExprP
lamP = try $ LamP <$> maybeName <*> inTypeP <* char '⇒' <*> exprP

funcP :: Parsec String st ExprP
funcP = choice [paren exprP, starP, refP]

sepBy2 :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepBy2 it sep = (:) <$> it <* sep <*> it `sepBy1` sep

appP :: Parsec String st ExprP
appP = try $ AppP <$> funcP `sepBy2` try (spaces1 <* (notFollowedBy $ oneOf $ bichars ++ ")"))

inTypeP :: Parsec String st ExprP
inTypeP = spaces *> choice [appP, paren exprP, starP, refP] <* spaces

exprP :: Parsec String st ExprP
exprP = spaces *> choice [lamP, piP, nameP, typeP, appP, paren exprP, starP, refP] <* spaces

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
