{-#OPTIONS_GHC -Wall #-}

module Candid.Expression
  ( Expression(..)
  , hole
  , Context
  , pretty
  , closed
  , hashOf
  , holesIn
  , nameOf
  , typeOf
  , withType
  , shift
  , replace
  , reduce
  , applicate
  , equiv
  ) where

import qualified Blake2s1 as H
import Data.Maybe (listToMaybe)

data Expression
  = Star
  | Box
  | Hole String
  | Ref Expression Int
  | Pi String Expression Expression
  | Lambda Expression String Expression Expression
  | Name Expression String Expression
  | Apply Expression Expression Expression
  | Hash Expression String H.Hash
  deriving (Show, Read)

hole :: Expression
hole = Hole ""

type Context = [Expression]

showName :: String -> String -> String
showName "" _ = ""
showName name sep = name ++ sep

parenLevel :: Expression-> Int
parenLevel Star = 10
parenLevel Box = 10
parenLevel (Hole _) = 10
parenLevel (Ref _ _) = 10
parenLevel (Name _ _ _) = 5
parenLevel (Pi _ _ _) = 5
parenLevel (Lambda _ _ _ _) = 5
parenLevel (Apply _ _ _) = 9
parenLevel (Hash _ _ _) = 10

paren :: Bool -> String -> String
paren True s = "(" ++ s ++ ")"
paren False s = s

pretty :: Context -> Expression-> String
pretty ctx =
  let prettyErrors :: Expression -> String
      prettyErrors expr = case typeOf expr of
                               Hole "" -> ""
                               Hole msg -> "{- " ++ msg ++ " -} "
                               _ -> ""
      pretty' level expr = paren (level >= parenLevel expr) $ prettyErrors expr ++
        case expr of
             Star -> "★"
             Box -> "□"
             Hole s -> '?':s
             Ref _ n -> maybe ('!':show n) nameOf $ listToMaybe $ drop n ctx
             Pi name inType outType -> showName name ":" ++ pretty' 5 inType ++ " → " ++ pretty (expr:ctx) outType
             Lambda _ name inType body -> showName name ":" ++ pretty' 5 inType ++ " ⇒ " ++ pretty (expr:ctx) body
             Name _ name body -> showName name " = " ++ pretty (expr:ctx) body
             Apply _ function argument -> pretty' 5 function ++ " " ++ pretty' 9 argument
             Hash _ name _ -> name
   in pretty' 0

closed :: Expression -> Bool
closed =
  let rec :: Int -> Expression -> Bool
      rec d expr =
        case expr of
             Star -> True
             Box -> True
             Hole _ -> False
             Ref _ n -> 0 <= n && n < d
             Pi _ iT oT -> rec d iT && rec (d+1) oT
             Lambda _ _ iT b -> rec d iT && rec (d+1) b
             Name _ _ b -> rec (d+1) b
             Apply _ f a -> rec d f && rec d a
             Hash _ _ _ -> True
   in rec 0

hashOf :: Expression -> H.Hash
hashOf expr =
  case expr of
       Star -> H.hash H.zero H.zero (0xffffffff,0,0,1)
       Box -> H.hash H.zero H.zero (0xffffffff,0,0,2)
       Hole _ -> H.hash H.zero H.zero (0xffffffff,0,0,0)
       Ref _ n -> H.hash H.zero H.zero (1,0,0,fromIntegral n)
       Pi _ iT oT -> H.hash (hashOf iT) (hashOf oT) (0,0,0,3)
       Lambda _ _ iT b -> H.hash (hashOf iT) (hashOf b) (0,0,0,2)
       Name _ _ b -> if closed b then (hashOf b) else H.hash H.zero (hashOf b) (0,0,0,4)
       Apply _ f a -> H.hash (hashOf f) (hashOf a) (0,0,0,1)
       Hash _ _ h -> h

holesIn :: Expression -> [String]
holesIn =
  let holes % expr = 
        case expr of
             Star -> holes
             Box -> holes
             Hole name -> name : holes
             Ref ty _ -> holes % ty
             Pi _ iT oT -> holes % iT % oT
             Lambda ty _ iT b -> holes % ty % iT % b
             Name ty _ b -> holes % ty % b
             Apply ty f a -> holes % ty % f % a
             Hash ty _ _ -> holes % ty
   in ([] %)

nameOf :: Expression -> String
nameOf expr =
  case expr of
       Star -> ""
       Box -> ""
       Hole _ -> ""
       Ref _ _ -> ""
       Pi name _ _ -> name
       Lambda _ name _ _ -> name
       Name _ name _ -> name
       Apply _ _ _ -> ""
       Hash _ name _ -> name

typeOf :: Expression -> Expression
typeOf expr =
  case expr of
       Star -> Box
       Box -> hole
       Hole _ -> hole
       Ref ty _ -> ty
       Pi _ _ outType -> typeOf $ outType
       Lambda ty name inType body ->
         case ty of
              Hole _ -> Pi name inType $ typeOf body
              _ -> ty
       Name ty _ body ->
         case ty of
              Hole _ -> replace expr $ typeOf body
              _ -> ty
       Apply ty _ _ -> ty
       Hash ty _ _ -> ty

withType :: Expression -> Expression -> Expression
withType expr ty =
  case expr of
       Star -> Star
       Box -> Box
       Hole name -> Hole name
       Ref _ n -> Ref ty n
       Pi name inType outType -> Pi name inType outType
       Lambda _ name inType body -> Lambda ty name inType body
       Name _ name body -> Name ty name body
       Apply _ function argument -> Apply ty function argument
       Hash _ name hash -> Hash ty name hash

shift :: (Int -> Int) -> Expression -> Expression
shift adj =
  let rec depth expr =
        case expr of
             Star -> Star
             Box -> Box
             Hole name -> Hole name
             Ref ty n -> Ref (rec depth ty) $ if depth <= n then adj n else n
             Pi name inType outType -> Pi name (rec depth inType) (rec (1+depth) outType)
             Lambda ty name inType body -> Lambda (rec depth ty) name (rec depth inType) (rec (1+depth) body)
             Name ty name body -> Name (rec depth ty) name (rec (1+depth) body)
             Apply ty function argument -> Apply (rec depth ty) (rec depth function) (rec depth argument)
             Hash ty name hash -> Hash (rec depth ty) name hash
   in rec 0

replace :: Expression -> Expression -> Expression
replace ref =
  let rec depth expr =
        case expr of
             Star -> Star
             Box -> Box
             Hole name -> Hole name
             Ref _ n -> if depth == n then shift (1 + depth +) ref else expr -- FIXME check type?
             Pi name inType outType -> Pi name (rec depth inType) (rec (1+depth) outType)
             Lambda ty name inType body -> Lambda (rec depth ty) name (rec depth inType) (rec (1+depth) body)
             Name ty name body -> Name (rec depth ty) name (rec (1+depth) body)
             Apply ty function argument -> Apply (rec depth ty) (rec depth function) (rec depth argument)
             Hash ty name hash -> Hash (rec depth ty) name hash
   in shift (-1 +) . rec 0

reduce :: Expression -> Expression
reduce expr =
      case expr of
           Pi name inType outType -> Pi name (reduce inType) (reduce outType)
           Lambda ty name inType body -> Lambda ty name (reduce inType) (reduce body)
           Name ty name body -> Name ty name (reduce body)
           Apply ty function argument -> 
             let rf = reduce function
                 ra = reduce argument
              in case rf of
                      Name _ _ body -> reduce $ Apply ty (replace rf body) ra
                      Lambda _ _ _ body -> reduce $ replace argument body
                      _ -> Apply ty ra rf
           _ -> expr

applicate :: (H.Hash -> Maybe Expression) -> Expression -> Expression
applicate hashExpr expr =
  case expr of
       Apply ty function argument ->
         let function' = applicate hashExpr function
          in case function' of
                  Lambda _ _ _ body -> applicate hashExpr $ replace argument body
                  _ -> Apply ty function' argument
       Hash _ _ h ->
         maybe expr (applicate hashExpr) $ hashExpr h
       Name _ _ body ->
         applicate hashExpr $ replace expr body
       _ -> expr

-- are the two expressions equivalent
equiv :: (H.Hash -> Maybe Expression) -> Expression -> Expression -> Bool
equiv hashExpr =
  let eq x y =
        case (x,y) of
             (Star, Star) -> True
             (Box, Box) -> True
             (Hole _, Hole _) -> True
             (Ref _ nX, Ref _ nY) -> nX == nY
             (Name _ _ bX, Name _ _ bY) -> eq bX bY
             (Pi _ inTypeX outTypeX, Pi _ inTypeY outTypeY) ->
               eq inTypeX inTypeY && eq outTypeX outTypeY
             (Lambda _ _ inTypeX bodyX, Lambda _ _ inTypeY bodyY) ->
               eq inTypeX inTypeY && eq bodyX bodyY
             (Apply _ functionX argumentX, Apply _ functionY argumentY) ->
               eq functionX functionY && eq argumentX argumentY
             (Hash _ _ hX, Hash _ _ hY) -> hX == hY
             -- cases that don't match right off
             (Hash _ _ h, _) -> case hashExpr h of
                                     Nothing -> h == hashOf y
                                     Just x' -> eq x' y
             (_, Hash _ _ h) -> case hashExpr h of
                                     Nothing -> h == hashOf x
                                     Just y' -> eq x y'
             (Name _ _ b, _) -> eq (replace x b) y
             (_, Name _ _ b) -> eq x (replace y b)
             (_, _) -> False

   in eq
