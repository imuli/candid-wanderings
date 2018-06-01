module Candid.Typecheck
  ( TypeError(..)
  , typecheck
  ) where

import Data.Maybe (listToMaybe)
import Candid.Expression
import qualified Blake2s1 as H

data TypeError
  = TypeMismatch (Context Int) (Expression Int) (Expression Int) (Expression Int)
  | HasHole (Context Int) (Expression Int)
  | OpenExpression (Context Int) (Expression Int)
  | TypeInference (Context Int) (Expression Int)
  deriving (Show)

index :: Int -> [a] -> Maybe a
index i = listToMaybe . drop i

typecheck :: (H.Hash -> Expression Int) -> (H.Hash -> Expression Int) -> Bool -> Bool -> Context Int -> Expression Int -> Either TypeError (Expression Int)
typecheck hashExpr hashType holeOk = tc
  where
    isStar ctx parent Star = Right Star
    isStar ctx parent expr = Left $ TypeMismatch ctx parent Star expr
    tc trust ctx expr =
      case (trust,expr) of
           (True, Rec i) -> Left $ TypeInference ctx expr
           (True, Pi _ _ _ _) -> Right Star
           (True, Assert _ outType _) -> Right outType
           (_, Hole s) -> if holeOk
                             then Right expr
                             else Left $ HasHole ctx expr
           (_, Star) -> Right Star
           (_, Ref n) -> case index n ctx of
                              Nothing -> Left $ OpenExpression ctx expr
                              Just x -> Right $ shift (n+1+) $ inTypeOf x
           (_, Rec n) -> case index n ctx of
                              Nothing -> Left $ OpenExpression ctx expr
                              Just x -> tc True (drop n ctx) x
           (_, Pi _ _ inType outType) ->
             tc trust ctx inType >>= isStar ctx expr >>
             tc trust (expr:ctx) outType >>= isStar ctx expr
           (_, Assert _ outType body) ->
             tc trust ctx outType >>= isStar ctx expr >>
             tc trust ctx body >>= (\actual ->
               if equiv hashExpr ctx outType ctx actual
                  then Right outType
                  else Left (TypeMismatch ctx body outType actual))
           (_, Apply _ func arg) ->
             tc trust ctx func >>= \funcType -> tc trust ctx arg >>= \argType ->
               case funcType of
                    Pi _ _ inType outType -> 
                      if equiv hashExpr ctx inType ctx argType
                         then Right (replace arg funcType outType)
                         else Left (TypeMismatch ctx arg inType argType)
                    _ -> Left $ TypeMismatch ctx func (Pi "" "" argType $ Hole "") funcType

           (_, Lambda _ bn inType body) ->
             tc trust ctx inType >>= isStar ctx expr >>
             tc trust (expr:ctx) body >>= \outType -> Right $ Pi "" bn inType outType
