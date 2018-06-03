module Candid.Typecheck
  ( TypeError(..)
  , typecheck
  , prettyError
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


prettyError :: TypeError -> String
prettyError err =
  case err of
       TypeMismatch ctx expr expected actual -> "Type mismatch" ++ ctxMsg ctx ++ " at " ++ pretty ctx expr ++ "\n\tExpected type: " ++ pretty ctx expected ++ "\n\tActual type: " ++ pretty ctx actual
       HasHole ctx expr -> "Hole " ++ pretty ctx expr ++ ctxMsg ctx
       OpenExpression ctx expr -> "Open Expression " ++ pretty ctx expr ++ ctxMsg ctx
       TypeInference ctx expr -> "Type Inference with " ++ pretty ctx expr ++ ctxMsg ctx
 where
   ctxMsg :: Context Int -> String
   ctxMsg [] = ""
   ctxMsg (x:xs) = " in expression `" ++ pretty xs x ++ "`"

index :: Int -> [a] -> Maybe a
index i = listToMaybe . drop i

typecheck :: (H.Hash -> Maybe (Expression Int)) -> (H.Hash -> Maybe (Expression Int)) -> Bool -> Bool -> Context Int -> Expression Int -> Either TypeError (Expression Int)
typecheck hashExpr hashType holeOk = tc
  where
    unwrapType :: Expression Int -> Either a (Expression Int)
    unwrapType x = Right $ case x of
                        Hash _ h -> maybe x id $ hashExpr h
                        _ -> x
    isStar _ _ Star = Right Star
    isStar ctx x ty = Left $ TypeMismatch ctx x Star ty
    tc trust ctx expr =
      case (trust,expr) of
           (True, Rec _) -> Left $ TypeInference ctx expr
           (True, Pi _ _ _ _) -> Right Star
           (True, Assert _ outType _) -> Right outType
           (_, Hole _) -> if holeOk
                             then Right expr
                             else Left $ HasHole ctx expr
           (_, Star) -> Right Star
           (_, Ref n) -> case index n ctx of
                              Nothing -> Left $ OpenExpression ctx expr
                              Just x -> Right $ shift (n+1+) $ inTypeOf x
           (_, Rec n) -> case index n ctx of
                              Nothing -> Left $ OpenExpression ctx expr
                              Just x -> tc True (drop (n+1) ctx) x >>= Right . (shift (n+1+))
           (_, Pi _ _ inType outType) ->
             tc trust ctx inType >>= isStar ctx inType >>
             tc trust (expr:ctx) outType >>= isStar ctx outType
           (_, Assert _ outType body) ->
             tc trust ctx outType >>= isStar ctx outType >>
             tc trust ctx body >>= (\actual ->
               if equiv hashExpr ctx outType ctx actual
                  then Right outType
                  else Left (TypeMismatch ctx body outType actual))
           (_, Apply _ func arg) ->
             tc trust ctx func >>= unwrapType >>= \funcType -> tc trust ctx arg >>= \argType ->
               case applicate hashExpr funcType of
                    Pi _ _ inType outType ->
                      if equiv hashExpr ctx inType ctx argType
                         then Right (replace arg funcType outType)
                         else Left (TypeMismatch ctx arg inType argType)
                    _ -> Left $ TypeMismatch ctx func (Pi "" "" argType $ Hole "") funcType
           (_, Lambda _ bn inType body) ->
             tc trust ctx inType >>= isStar ctx inType >>
             tc trust (expr:ctx) body >>= \outType -> Right $ Pi "" bn inType outType
           (_, Hash name h) -> Right $ maybe (Hole name) id $ hashType h
