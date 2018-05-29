{-#OPTIONS_GHC -Wall #-}

module Candid.Expression
  ( Expression(..)
  , Context
  , closed
  , hash
  , nameOf
  , boundNameOf
  ) where

import qualified Blake2s1 as H

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

