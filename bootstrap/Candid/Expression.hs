{-#OPTIONS_GHC -Wall #-}

module Candid.Expression
  ( Expression(..)
  , Context
  , closed
  , hash
  , inTypeOf
  , nameOf
  , recurseContext
  , shift
  , replace
  , reduce
  , equiv
  , pretty
  , applicate
  ) where

import qualified Blake2s1 as H
import Data.Maybe (listToMaybe)

data Expression
  = Star
  | Ref Int
  | Hole String
  | Name String Expression
  | Pi String Expression Expression
  | Lambda String Expression Expression
  | Apply Expression Expression
  | Assert Expression Expression
  | Hash String H.Hash
  deriving (Show, Read)

type Context = [Expression]

showName :: String -> String -> String
showName "" _ = ""
showName name sep = name ++ sep

parenLevel :: Expression-> Int
parenLevel Star = 10
parenLevel (Ref _) = 10
parenLevel (Hole _) = 10
parenLevel (Name _ _) = 5
parenLevel (Pi _ _ _) = 5
parenLevel (Lambda _ _ _) = 5
parenLevel (Apply _ _) = 9
parenLevel (Assert _ _) = 5
parenLevel (Hash _ _) = 10

paren :: Bool -> String -> String
paren True s = "(" ++ s ++ ")"
paren False s = s

pretty :: Context -> Expression-> String
pretty ctx = pretty' 0
  where
    pretty' level x = paren (level >= parenLevel x) $
      case x of
           Star -> "*"
           Hole s -> '?':s
           Ref n -> maybe ('!':show n) nameOf $ listToMaybe $ drop n ctx
           Pi name inType outType -> showName name ":" ++ pretty' 5 inType ++ " → " ++ pretty (x:ctx) outType
           Name name body -> showName name " = " ++ pretty (x:ctx) body
           Lambda name inType body -> showName name ":" ++ pretty' 5 inType ++ " ⇒ " ++ pretty (x:ctx) body
           Apply function argument -> pretty' 5 function ++ " " ++ pretty' 9 argument
           Assert outType body -> pretty' 5 outType ++ " ~\n\t" ++ pretty' 0 body
           Hash name _ -> name

hash :: Expression -> H.Hash
hash Star = H.hash H.zero H.zero (-1,0,0,1)
hash (Hole _) = H.hash H.zero H.zero (-1,0,0,0)
hash (Ref n) = H.hash H.zero H.zero (1,0,0,fromIntegral n)
hash (Name _ b) = H.hash H.zero (hash b) (0,0,0,4)
hash (Pi _ iT oT) = H.hash (hash iT) (hash oT) (0,0,0,3)
hash (Lambda _ iT b) = H.hash (hash iT) (hash b) (0,0,0,2)
hash (Apply f a) = H.hash (hash f) (hash a) (0,0,0,1)
hash (Assert _ b) = hash b
hash (Hash _ h) = h

closed :: Expression -> Bool
closed = closed' 0
 where
  closed' :: Int -> Expression -> Bool
  closed' d (Ref n) = n /= (-1) && d > n
  closed' _ (Hole _) = False
  closed' d (Pi _ iT oT) = closed' d iT && closed' (d+1) oT
  closed' d (Lambda _ iT b) = closed' d iT && closed' (d+1) b
  closed' d (Apply f a) = closed' d f && closed' d a
  closed' d (Assert oT b) = closed' d oT && closed' d b
  closed' _ _ = True

inTypeOf :: Expression -> Expression
inTypeOf (Pi _ t _) = t
inTypeOf (Lambda _ t _) = t
inTypeOf (Name _ b) = inTypeOf b
inTypeOf _ = Hole ""

nameOf :: Expression -> String
nameOf (Name n _) = n
nameOf (Pi n _ _) = n
nameOf (Lambda n _ _) = n
nameOf (Hash n _) = n
nameOf _ = ""

recurseContext :: r -> (Context -> Expression -> r -> r -> r) -> Context -> Expression -> r
recurseContext def f ctx expr = uncurry (f ctx expr) $
  case expr of
       Name _ body -> (def, recWith body)
       Pi _ iT oT -> (rec iT, recWith oT)
       Lambda _ iT body -> (rec iT, recWith body)
       Apply func arg -> (rec func, rec arg)
       Assert oT body -> (rec oT, rec body)
       _ -> (def, def)
   where
     rec = recurseContext def f ctx
     recWith = recurseContext def f (expr:ctx)

shift :: (Int -> Int) -> Expression -> Expression
shift adj = recurseContext Star shift' []
  where
    shift' ctx expr left right = case expr of
                                      Ref n -> Ref $ if length ctx <= n then adj n else n
                                      Name nm _ -> Name nm right
                                      Pi nm _ _ -> Pi nm left right
                                      Lambda nm _ _ -> Lambda nm left right
                                      Assert _ _ -> Assert left right
                                      Apply _ _ -> Apply left right
                                      _ -> expr

replace :: Expression -> Expression -> Expression
replace ref = shift (-1 +) . recurseContext Star replace' []
  where
    replace' ctx expr left right = case expr of
                                        Ref n -> if depth == n
                                                    then shift (1 + depth +) ref
                                                    else expr
                                        Name nm _ -> Name nm right
                                        Pi nm _ _ -> Pi nm left right
                                        Lambda nm _ _ -> Lambda nm left right
                                        Assert _ _ -> Assert left right
                                        Apply _ _ -> Apply left right
                                        _ -> expr
      where
        depth = fromIntegral $ length ctx

reduce :: Expression -> Expression
reduce = recurseContext Star reduce' []
  where
    reduce' _ expr left right = case expr of
                                     Name nm _ -> Name nm right
                                     Pi nm _ _ -> Pi nm left right
                                     Lambda nm _ _ -> Lambda nm left right
                                     Assert _ _ -> Assert left right
                                     Apply _ _ -> case left of
                                                       Name _ body ->
                                                         reduce $ Apply (replace expr body) right
                                                       Lambda _ _ body ->
                                                         reduce $ replace right body
                                                       _ -> Apply left right
                                     _ -> expr

applicate :: (H.Hash -> Maybe Expression) -> Expression -> Expression
applicate hashExpr x =
  case x of
       Apply function argument ->
         let expr = applicate hashExpr function
          in case expr of
                  Name _ body -> applicate hashExpr $ Apply (replace expr body) argument
                  Lambda _ _ body -> applicate hashExpr $ replace argument body
                  _ -> Apply expr argument
       Hash _ h -> maybe x id $ hashExpr h
       Assert _ body -> applicate hashExpr body
       _ -> x

-- are the two expressions equivalent, given their contexts
equiv :: (H.Hash -> Maybe Expression) -> Context -> Expression -> Context -> Expression -> Bool
equiv hashExpr = eq
  where
    eq cx x cy y =
      case (x,y) of
           (Star, Star) -> True
           (Hole _, Hole _) -> True
           (Name _ bX, Name _ bY) -> eq (x:cx) bX (y:cy) bY
           (Ref nX, Ref nY) -> nX == nY
           (Pi _ inTypeX outTypeX, Pi _ inTypeY outTypeY) ->
             eq cx inTypeX cy inTypeY && eq (x:cx) outTypeX (y:cy) outTypeY
           (Lambda _ inTypeX bodyX, Lambda _ inTypeY bodyY) ->
             eq cx inTypeX cy inTypeY && eq (x:cx) bodyX (y:cy) bodyY
           (Apply functionX argumentX, Apply functionY argumentY) ->
             eq cx functionX cy functionY && eq cx argumentX cy argumentY ||
               eq cx (reduce x) cy (reduce y)
           (Assert outTypeX bodyX, Assert outTypeY bodyY) ->
             eq cx outTypeX cy outTypeY && eq cx bodyX cy bodyY
           (Hash _ hX, Hash _ hY) -> hX == hY
           -- cases that don't match right off
           (Hash _ h, _) -> case hashExpr h of
                                 Nothing -> h == hash y
                                 Just x' -> eq cx x' cy y
           (_, Hash _ h) -> case hashExpr h of
                                 Nothing -> h == hash x
                                 Just y' -> eq cx x cy y'
           (Name _ b, _) -> eq cx (shift (-1 +) b) cy y
           (_, Name _ b) -> eq cx x cy (shift (-1 +) b)
           (Apply _ _, _) -> eq cx (applicate hashExpr x) cy y
           (_, Apply _ _) -> eq cx x cy (applicate hashExpr y)
           (_, _) -> False

