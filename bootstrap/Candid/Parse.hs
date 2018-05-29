module Candid.Parse where

import Candid.Expression
import Candid.Store
import Text.Parsec
import Text.Parsec.Token
import Data.Maybe (listToMaybe)

cSpaces1 :: Parsec String st ()
cSpaces1 = () <$ optional (try (many (oneOf " \t\r") <* newline)) <* many1 (oneOf " \t")

cSpaces :: Parsec String st ()
cSpaces = () <$ many (oneOf " \t\r") <* optional (try (newline <* many1 (oneOf " \t")))

binaries :: String
binaries = ":=~⇒→"

nullaries :: String
nullaries = "*?"

nameChar :: Parsec String st Char
nameChar = noneOf (" \t\r\n()" ++ nullaries ++ binaries)

rename :: String -> (Expression t) -> (Expression t)
rename name expr = case expr of
  Pi _ bn i o -> Pi name bn i o
  Lambda _ bn i b -> Lambda name bn i b
  Assert _ i b -> Assert name i b
  Apply _ f a -> Apply name f a
  _ -> expr

pStar :: Parsec String st (Expression String)
pStar = try $ Star <$ char '*'

pHole :: Parsec String st (Expression String)
pHole = try $ Hole <$> (char '?' *> many nameChar)

pName :: Char -> Parsec String st String
pName c = try (cSpaces *> many1 nameChar <* cSpaces <* char c) <|> string ""

-- actually reference or recurence, as they are not distinguishable here
pRef :: Parsec String st (Expression String)
pRef = Ref <$> many1 nameChar

pRename :: Parsec String st (Expression String) -> Parsec String st (Expression String)
pRename p = rename <$> pName '=' <*> p

pPi :: Parsec String st (Expression String)
pPi = try $ Pi "" <$> pName ':' <*> pExpr' <*> (cSpaces *> char '→' *> pExpr)

pLambda :: Parsec String st (Expression String)
pLambda = try $ Lambda "" <$> pName ':' <*> pExpr' <*> (cSpaces *> char '⇒' *> pExpr)

pAssert :: Parsec String st (Expression String)
pAssert = try $ Assert "" <$> pExpr' <*> (cSpaces *> char '~' *> pExpr)

pAppList :: Parsec String st [Expression String]
pAppList = funcs `sepBy1` (try (cSpaces1 <* notFollowedBy (oneOf (binaries ++ ")"))))
  where
    funcs = cSpaces *> choice [paren pExpr, pRef, pStar, pHole]

applyFromList :: [Expression t] -> Expression t
applyFromList = afl . reverse
  where
    afl (x : []) = x
    afl (x : xs) = Apply "" (afl xs) x

pApply :: Parsec String st (Expression String)
pApply = applyFromList <$> try pAppList

paren :: Parsec String st u -> Parsec String st u
paren inner = try $ char '(' *> inner <* (cSpaces *> char ')')

pExpr' :: Parsec String st (Expression String)
pExpr' = cSpaces *> choice [pApply, (paren pExpr), pRef, pStar, pHole]

pExpr :: Parsec String st (Expression String)
pExpr = cSpaces *> pRename (choice [pLambda, pPi, pAssert, pApply, (paren pExpr), pRef, pStar, pHole])

pExprs :: Parsec String st [Maybe (Expression String)]
pExprs = optionMaybe pExpr `sepBy` char '\n'

pFile :: Parsec String st [Maybe (Expression String)]
pFile = pExprs <* eof

-- search for a name in context
search :: String -> Context String -> Maybe (Expression Word)
search str = search' 0 where
  search' :: Word -> Context String -> Maybe (Expression Word)
  search' _ []        = Nothing
  search' n (x : ctx) = if str == nameOf x then Just (Rec n)
                        else if str == boundNameOf x then Just (Ref n)
                        else search' (n+1) ctx

-- look up a name in context and store
find :: Store -> Context String -> String -> Maybe (Expression Word)
find store ctx str =
    case search str ctx of
         Just expr -> Just expr
         Nothing -> fmap expr $ listToMaybe $ byName store str

-- translate from Expression String to Expression Word
number :: Store -> Expression String -> Expression Word
number store = num []
 where
  withName nm fn expr = if nm == ""
                           then fn expr
                           else rename (nameOf expr) $ fn $ rename nm expr
  num :: Context String -> Expression String -> Expression Word
  num ctx = num'
   where
   num' expr = case expr of
     Star -> Star
     Rec s -> case find store ctx s of
                   Nothing -> Hole s
                   Just repl -> repl
     Ref s -> case find store ctx s of
                   Nothing -> Hole s
                   Just repl -> repl
     Pi name bindName inType outType -> Pi name bindName (num' inType) (num (expr:ctx) outType)
     Lambda name bindName inType body -> Lambda name bindName (num' inType) (num (expr:ctx) body)
     Apply name function argument -> Apply name (num' function) (num' argument)
     Assert name outType body -> Assert name (num' outType) (withName name num' body)
     Hash n h -> Hash n h
     Hole s -> Hole s

-- translate and save a list of expressions
numberInto :: Store -> [Expression String] -> Store
numberInto store = foldl into store
  where
    into :: Store -> Expression String -> Store
    into store expr = let numExpr = number store expr
                          (h, newStore) = add store numExpr
                       in newStore

parseText :: String -> Either ParseError [Maybe (Expression String)]
parseText = parse pFile ""
