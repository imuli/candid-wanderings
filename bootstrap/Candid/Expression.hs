{-#OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

module Candid.Expression
  ( Expression(..)
  , Context
  , closed
  , hash
  , inTypeOf
  , nameOf
  , boundNameOf
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

data Expression t
  = Star
  | Hole String
  | Ref t
  | Rec t
  | Pi String String (Expression t) (Expression t)
  | Lambda String String (Expression t) (Expression t)
  | Apply String (Expression t) (Expression t)
  | Assert String (Expression t) (Expression t)
  | Hash String H.Hash
  deriving (Show)

type Context t = [Expression t]

showName :: String -> String -> String
showName "" _ = ""
showName name sep = name ++ sep

parenLevel :: Expression a -> Int
parenLevel Star = 10
parenLevel (Hole _) = 10
parenLevel (Ref _) = 10
parenLevel (Rec _) = 10
parenLevel (Pi _ _ _ _) = 5
parenLevel (Lambda _ _ _ _) = 5
parenLevel (Apply _ _ _) = 9
parenLevel (Assert _ _ _) = 5
parenLevel (Hash _ _) = 10

paren :: Bool -> String -> String
paren True s = "(" ++ s ++ ")"
paren False s = s

pretty :: Integral a => Show a => Context a -> Expression a -> String
pretty ctx = pretty' 0
  where
    pretty' level x = paren (level >= parenLevel x) $
      case x of
           Star -> "*"
           Hole s -> '?':s
           Ref n -> maybe ('!':show n) boundNameOf $ listToMaybe $ drop (fromIntegral n) ctx
           Rec n -> maybe ('@':show n) nameOf $ listToMaybe $ drop (fromIntegral n) ctx
           Pi name boundName inType outType -> showName name " = " ++ showName boundName ":" ++ pretty' 5 inType ++ " → " ++ pretty (x:ctx) outType
           Lambda name boundName inType body -> showName name " = " ++ showName boundName ":" ++ pretty' 5 inType ++ " ⇒ " ++ pretty (x:ctx) body
           Apply name function argument -> showName name " = " ++ pretty' 5 function ++ " " ++ pretty' 9 argument
           Assert name outType body -> showName name " = " ++ pretty' 5 outType ++ " ~\n\t" ++ pretty' 0 body
           Hash name _ -> name

hash :: Integral a => Expression a -> H.Hash
hash Star = H.hash H.zero H.zero (-1,0,0,1)
hash (Hole _) = H.hash H.zero H.zero (-1,0,0,0)
hash (Ref n) = H.hash H.zero H.zero (1,0,0,fromIntegral n)
hash (Rec n) = H.hash H.zero H.zero (2,0,0,fromIntegral n)
hash (Pi _ _ iT oT) = H.hash (hash iT) (hash oT) (0,0,0,3)
hash (Lambda _ _ iT b) = H.hash (hash iT) (hash b) (0,0,0,2)
hash (Apply _ f a) = H.hash (hash f) (hash a) (0,0,0,1)
hash (Assert _ _ b) = hash b
hash (Hash _ h) = h

closed :: Integral t => Expression t -> Bool
closed = closed' 0
 where
  closed' :: Integral t => t -> Expression t -> Bool
  closed' d (Ref n) = n /= (-1) && d > n
  closed' d (Rec n) = n /= (-1) && d > n
  closed' _ (Hole _) = False
  closed' d (Pi _ _ iT oT) = closed' d iT && closed' (d+1) oT
  closed' d (Lambda _ _ iT b) = closed' d iT && closed' (d+1) b
  closed' d (Apply _ f a) = closed' d f && closed' d a
  closed' d (Assert _ oT b) = closed' d oT && closed' d b
  closed' _ _ = True

inTypeOf :: Expression t -> Expression t
inTypeOf (Pi _ _ t _) = t
inTypeOf (Lambda _ _ t _) = t
inTypeOf _ = Hole ""

nameOf :: Expression t -> String
nameOf (Pi n _ _ _) = n
nameOf (Lambda n _ _ _) = n
nameOf (Apply n _ _) = n
nameOf (Assert n _ _) = n
nameOf (Hash n _) = n
nameOf _ = ""

boundNameOf :: Expression t -> String
boundNameOf (Pi _ bn _ _) = bn
boundNameOf (Lambda _ bn _ _) = bn
boundNameOf _ = ""

recurseContext :: r -> (Context t -> Expression t -> r -> r -> r) -> Context t -> Expression t -> r
recurseContext def f ctx expr = uncurry (f ctx expr) $
  case expr of
       Pi _ _ iT oT -> (rec iT, recWith oT)
       Lambda _ _ iT body -> (rec iT, recWith body)
       Apply _ func arg -> (rec func, rec arg)
       Assert _ oT body -> (rec oT, rec body)
       _ -> (def, def)
   where
     rec = recurseContext def f ctx
     recWith = recurseContext def f (expr:ctx)

shift :: Integral t => (t -> t) -> Expression t -> Expression t
shift adj = recurseContext Star shift' []
  where
    shift' ctx expr left right = case expr of
                                      Ref n -> Ref $ if length ctx <= fromIntegral n then adj n else n
                                      Rec n -> Rec $ if length ctx <= fromIntegral n then adj n else n
                                      Pi nm bnm _ _ -> Pi nm bnm left right
                                      Lambda nm bnm _ _ -> Lambda nm bnm left right
                                      Assert nm _ _ -> Assert nm left right
                                      Apply nm _ _ -> Apply nm left right
                                      _ -> expr

replace :: Integral t => Expression t -> Expression t -> Expression t -> Expression t
replace ref rec = shift (-1 +) . recurseContext Star replace' []
  where
    replace' ctx expr left right = case expr of
                                        Ref n -> if depth == n
                                                    then shift (1 + depth +) ref
                                                    else expr
                                        Rec n -> if depth == n
                                                    then shift (1 + depth +) rec
                                                    else expr
                                        Pi nm bnm _ _ -> Pi nm bnm left right
                                        Lambda nm bnm _ _ -> Lambda nm bnm left right
                                        Assert nm _ _ -> Assert nm left right
                                        Apply nm _ _ -> Apply nm left right
                                        _ -> expr
      where
        depth = fromIntegral $ length ctx

reduce :: Integral t => Expression t -> Expression t
reduce = recurseContext Star reduce' []
  where
    reduce' _ expr left right = case expr of
                                     Pi nm bnm _ _ -> Pi nm bnm left right
                                     Lambda nm bnm _ _ -> Lambda nm bnm left right
                                     Assert nm _ _ -> Assert nm left right
                                     Apply nm _ _ -> case left of
                                                          Lambda _ _ _ body ->
                                                            reduce $ replace right left body
                                                          _ -> Apply nm left right
                                     _ -> expr

applicate :: (H.Hash -> Maybe (Expression Int)) -> Expression Int -> Expression Int
applicate hashExpr x =
  case x of
       Apply nm function argument ->
         case applicate hashExpr function of
              Lambda _ _ _ body -> replace argument function body
              y -> Apply nm y argument
       Hash _ h -> maybe x id $ hashExpr h
       _ -> x

-- are the two expressions equivalent, given their contexts
equiv :: (H.Hash -> Maybe (Expression Int)) -> Context Int -> Expression Int -> Context Int -> Expression Int -> Bool
equiv hashExpr = eq
  where
    eq cx x cy y =
      case (x,y) of
           (Star, Star) -> True
           (Hole _, Hole _) -> True
           (Ref nX, Ref nY) -> nX == nY
           (Rec nX, Rec nY) -> nX == nY
           (Pi _ _ inTypeX outTypeX, Pi _ _ inTypeY outTypeY) ->
             eq cx inTypeX cy inTypeY && eq (x:cx) outTypeX (y:cy) outTypeY
           (Lambda _ _ inTypeX bodyX, Lambda _ _ inTypeY bodyY) ->
             eq cx inTypeX cy inTypeY && eq (x:cx) bodyX (y:cy) bodyY
           (Apply _ functionX argumentX, Apply _ functionY argumentY) ->
             eq cx functionX cy functionY && eq cx argumentX cy argumentY ||
               eq cx (reduce x) cy (reduce y)
           (Assert _ outTypeX bodyX, Assert _ outTypeY bodyY) ->
             eq cx outTypeX cy outTypeY && eq cx bodyX cy bodyY
           (Hash _ hX, Hash _ hY) -> hX == hY
           -- cases that don't match right off
           (Rec n, _) -> case drop n cx of [] -> False; (x':cx') -> eq cx' x' cy y
           (_, Rec n) -> case drop n cy of [] -> False; (y':cy') -> eq cx x cy' y'
           (Hash _ h, _) -> case hashExpr h of
                                 Nothing -> h == hash y
                                 Just x' -> eq cx x' cy y
           (_, Hash _ h) -> case hashExpr h of
                                 Nothing -> h == hash x
                                 Just y' -> eq cx x cy y'
           (Apply _ _ _, _) -> eq cx (applicate hashExpr x) cy y
           (_, Apply _ _ _) -> eq cx x cy (applicate hashExpr y)
           (_, _) -> False

