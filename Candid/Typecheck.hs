{-#OPTIONS_GHC -Wall #-}
module Candid.Typecheck
  ( typeFill
  ) where

import Data.Maybe (listToMaybe)
import Candid.Expression
import qualified Blake2s1 as H

index :: Int -> [a] -> Maybe a
index i = listToMaybe . drop i

refTypeOf :: Expression -> Expression
refTypeOf expr =
  case expr of
       Name ty _ body ->
         case ty of
              Hole _ -> replace expr $ typeOf body
              _ -> ty
       Pi _ inType _ -> inType
       Lambda _ _ inType _ -> inType
       _ -> Hole "weird reference"

typeFill :: (H.Hash -> Maybe Expression) -> Context -> Expression -> Expression
typeFill hashExpr =
  let check ctx a b =
        case a of
             Hole _ -> b
             _ -> if equiv hashExpr a b then a else Hole $ "Failed type assertion: " ++ pretty ctx a ++ " was found as " ++ pretty ctx b ++ "."
      tf ctx expr =
        case expr of
             Star -> Star
             Box -> hole
             Hole _ -> expr
             Ref _ n -> case index n ctx of
                             Nothing -> Ref (Hole "open expression") n
                             Just ref -> Ref (tf ctx $ shift (n+1+) $ refTypeOf ref) n
             Name ty name body ->
               let ty' = tf ctx ty
                   body' = tf (Name ty' name body:ctx) body
                in Name (check ctx ty' $ typeOf $ replace (Name ty' name body') body') name body'
             Pi name inType outType ->
               let inType' = tf ctx inType
                   outType' = tf (Pi name inType' outType : ctx) outType
                in Pi name inType' outType'
             Lambda ty name inType body ->
               let ty' = tf ctx ty
                   inType' = tf ctx inType
                   body' = tf (Lambda ty' name inType' body : ctx) body
                in Lambda (check ctx ty' $ Pi name inType' $ typeOf body') name inType' body'
             Apply ty function argument ->
               let ty' = tf ctx ty
                   func' = tf ctx function
                   arg' = tf ctx argument
                in case applicate hashExpr $ typeOf func' of
                        Pi _ inType outType ->
                          if equiv hashExpr inType $ typeOf arg'
                             then Apply (check ctx ty' $ replace arg' outType) func' arg'
                             else Apply (Hole $ "Type mismatch (" ++
                               pretty ctx (typeOf func') ++ ") does not take (" ++
                                 pretty ctx (typeOf arg') ++ ").") func' arg'
                        Hole _ -> Apply hole func' arg'
                        _ ->
                          Apply (Hole $
                            "(" ++ pretty ctx (typeOf func') ++ ") is not a function type.") func' arg'
                   
             Hash ty name hash ->
               case hashExpr hash of
                    Nothing -> Hash (tf ctx ty) name hash
                    Just expr' -> Hash (typeOf expr') name hash
   in tf
