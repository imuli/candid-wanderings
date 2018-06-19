{-#OPTIONS_GHC -Wall #-}
module Candid.Parse where

import Candid.Expression
import Candid.Typecheck
import Candid.Store
import Text.Parsec
import Data.Maybe (listToMaybe)

cSpaces1 :: Parsec String st ()
cSpaces1 = () <$ optional (try (many (oneOf " \t\r") <* newline)) <* many1 (oneOf " \t")

cSpaces :: Parsec String st ()
cSpaces = () <$ many (oneOf " \t\r") <* optional (try (newline <* many1 (oneOf " \t")))

binaries :: String
binaries = ":=~⇒→"

nullaries :: String
nullaries = "*"

nameChar :: Parsec String st Char
nameChar = noneOf (" \t\r\n()" ++ nullaries ++ binaries)

pStar :: Parsec String st Expression
pStar = try $ Star <$ char '*'

pNm :: Char -> Parsec String st String
pNm c = try (cSpaces *> many1 nameChar <* cSpaces <* char c) <|> string ""

-- actually reference or recurence, as they are not distinguishable here
pRef :: Parsec String st Expression
pRef = Hole <$> many1 nameChar

pName :: Parsec String st Expression
pName = try $ Name <$> pNm '=' <*> pExpr

pPi :: Parsec String st Expression
pPi = try $ Pi <$> pNm ':' <*> pExpr' <*> (cSpaces *> char '→' *> pExpr)

pLambda :: Parsec String st Expression
pLambda = try $ Lambda <$> pNm ':' <*> pExpr' <*> (cSpaces *> char '⇒' *> pExpr)

pAssert :: Parsec String st Expression
pAssert = try $ Assert <$> pExpr' <*> (cSpaces *> char '~' *> pExpr)

pAppList :: Parsec String st [Expression]
pAppList = funcs `sepBy1` (try (cSpaces1 <* notFollowedBy (oneOf (binaries ++ ")"))))
  where
    funcs = cSpaces *> choice [paren pExpr, pRef, pStar]

applyFromList :: [Expression] -> Expression
applyFromList = afl . reverse
  where
    afl [] = Hole ""
    afl (x : []) = x
    afl (x : xs) = Apply (afl xs) x

pApply :: Parsec String st Expression
pApply = applyFromList <$> try pAppList

paren :: Parsec String st u -> Parsec String st u
paren inner = try $ char '(' *> inner <* (cSpaces *> char ')')

pExpr' :: Parsec String st Expression
pExpr' = cSpaces *> choice [pApply, (paren pExpr), pRef, pStar]

pExpr :: Parsec String st Expression
pExpr = cSpaces *> choice [pName, pLambda, pPi, pAssert, pApply, (paren pExpr), pRef, pStar]

pExprs :: Parsec String st [Maybe Expression]
pExprs = optionMaybe pExpr `sepBy` char '\n'

pFile :: Parsec String st [Maybe Expression]
pFile = pExprs <* eof

-- search for a name in context
search :: String -> Context -> Maybe Expression
search str = search' 0 where
  search' :: Int -> Context -> Maybe Expression
  search' _ []        = Nothing
  search' n (x : ctx) = if str == nameOf x then Just (Ref n)
                        else search' (n+1) ctx

-- look up a name in context and store
find :: Store -> Context -> String -> Maybe Expression
find store ctx str =
    case search str ctx of
         Just expr -> Just expr
         Nothing -> fmap (Hash str . entryHash) $ listToMaybe $ byName store str

-- translate from Expression to Expression
fillHoles :: Store -> Expression -> Expression
fillHoles store = num []
 where
  num :: Context -> Expression -> Expression
  num ctx = num'
   where
   num' expr = case expr of
     Star -> Star
     Ref n -> Ref n
     Hole s -> case find store ctx s of
                   Nothing -> Hole s
                   Just repl -> repl
     Name name body -> Name name (num (expr:ctx) body)
     Pi name inType outType -> Pi name (num' inType) (num (expr:ctx) outType)
     Lambda name inType body -> Lambda name (num' inType) (num (expr:ctx) body)
     Apply function argument -> Apply (num' function) (num' argument)
     Assert outType body -> Assert (num' outType) (num' body)
     Hash n h -> Hash n h


mapLeft :: (a->c) -> Either a b -> Either c b
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x

-- translate and save a list of expressions
numberInto :: Store -> [Expression] -> Either (String, TypeError) Store
numberInto = foldl into . Right
  where
    into :: Either (String, TypeError) Store -> Expression -> Either (String, TypeError) Store
    into est expr = est >>= \store -> fmap snd $ (mapLeft (\te -> (nameOf expr, te)) $ add store $ fillHoles store expr)

parseText :: String -> Either ParseError [Maybe Expression]
parseText = parse pFile ""
