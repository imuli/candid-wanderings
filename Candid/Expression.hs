{-#OPTIONS_GHC -Wall #-}

module Candid.Expression
  ( Expression(..)
  , Binder(..)
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
import Data.Char (chr, ord)

data Binder
  = Pi
  | Lambda
  | Fix
  deriving (Eq, Show, Read)

data Expression
  = Star Int
  | Hole String
  | Ref Expression Int
  | Bind Binder Expression String Expression Expression
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
parenLevel (Star _) = 10
parenLevel (Hole _) = 10
parenLevel (Ref _ _) = 10
parenLevel (Bind _ _ _ _ _) = 5
parenLevel (Apply _ _ _) = 9
parenLevel (Hash _ _ _) = 10

paren :: Bool -> String -> String
paren True s = "(" ++ s ++ ")"
paren False s = s

toSubscriptNumeral :: Char -> Char
toSubscriptNumeral = chr . (+ 0x2050) . ord

pretty :: Context -> Expression-> String
pretty ctx =
  let prettyErrors :: Expression -> String
      prettyErrors expr = case typeOf expr of
                               Hole "" -> ""
                               Hole msg -> "{- " ++ msg ++ " -} "
                               _ -> ""
      pretty' level expr = paren (level >= parenLevel expr) $ prettyErrors expr ++
        case expr of
             Star n -> '★':if n == 0 then "" else map toSubscriptNumeral $ show n
             Hole s -> '?':s
             Ref _ n -> maybe ('!':show n) nameOf $ listToMaybe $ drop n ctx
             Bind Pi _ name inType outType -> showName name ":" ++ pretty' 5 inType ++ " → " ++ pretty (expr:ctx) outType
             Bind Lambda _ name inType body -> showName name ":" ++ pretty' 5 inType ++ " ⇒ " ++ pretty (expr:ctx) body
             Bind Fix _ name _ body -> showName name " = " ++ pretty (expr:ctx) body
             Apply _ function argument -> pretty' 5 function ++ " " ++ pretty' 9 argument
             Hash _ name _ -> name
   in pretty' 0

closed :: Expression -> Bool
closed =
  let rec :: Int -> Expression -> Bool
      rec d expr =
        case expr of
             Star _ -> True
             Hole _ -> False
             Ref _ n -> 0 <= n && n < d
             Bind Pi _ _ iT oT -> rec d iT && rec (d+1) oT
             Bind Lambda _ _ iT b -> rec d iT && rec (d+1) b
             Bind Fix _ _ _ b -> rec (d+1) b
             Apply _ f a -> rec d f && rec d a
             Hash _ _ _ -> True
   in rec 0

hashOf :: Expression -> H.Hash
hashOf expr =
  case expr of
       Star n -> H.hash H.zero H.zero (0xffffffff,0,0,fromIntegral $ n+1)
       Hole _ -> H.hash H.zero H.zero (0xffffffff,0,0,0)
       Ref _ n -> H.hash H.zero H.zero (1,0,0,fromIntegral n)
       Bind Pi _ _ iT oT -> H.hash (hashOf iT) (hashOf oT) (0,0,0,3)
       Bind Lambda _ _ iT b -> H.hash (hashOf iT) (hashOf b) (0,0,0,2)
       Bind Fix _ _ _ b -> if closed b then (hashOf b) else H.hash H.zero (hashOf b) (0,0,0,4)
       Apply _ f a -> H.hash (hashOf f) (hashOf a) (0,0,0,1)
       Hash _ _ h -> h

holesIn :: Expression -> [String]
holesIn =
  let holes % expr = 
        case expr of
             Star _ -> holes
             Hole name -> name : holes
             Ref ty _ -> holes % ty
             Bind Pi _ _ iT oT -> holes % iT % oT
             Bind Lambda ty _ iT b -> holes % ty % iT % b
             Bind Fix ty _ _ b -> holes % ty % b
             Apply ty f a -> holes % ty % f % a
             Hash ty _ _ -> holes % ty
   in ([] %)

nameOf :: Expression -> String
nameOf expr =
  case expr of
       Star _ -> ""
       Hole _ -> ""
       Ref _ _ -> ""
       Bind Pi _ name _ _ -> name
       Bind Lambda _ name _ _ -> name
       Bind Fix _ name _ _ -> name
       Apply _ _ _ -> ""
       Hash _ name _ -> name

typeOf :: Expression -> Expression
typeOf expr =
  case expr of
       Star n -> Star (n+1)
       Hole _ -> hole
       Ref ty _ -> ty
       Bind Pi (Hole _) _ _ outType -> typeOf $ outType
       Bind Lambda (Hole _) name inType body -> Bind Pi (typeOf $ typeOf body) name inType $ typeOf body
       Bind Fix (Hole _) _ _ body -> replace expr $ typeOf body
       Bind _ ty _ _ _ -> ty
       Apply ty _ _ -> ty
       Hash ty _ _ -> ty

withType :: Expression -> Expression -> Expression
withType expr ty =
  case expr of
       Star _ -> expr
       Hole _ -> expr
       Ref _ n -> Ref ty n
       Bind Pi _ name inType outType -> Bind Pi ty name inType outType
       Bind Lambda _ name inType body -> Bind Lambda ty name inType body
       Bind Fix _ name ty' body -> Bind Fix ty name ty' body
       Apply _ function argument -> Apply ty function argument
       Hash _ name hash -> Hash ty name hash

shift :: (Int -> Int) -> Expression -> Expression
shift adj =
  let rec depth expr =
        case expr of
             Star _ -> expr
             Hole name -> Hole name
             Ref ty n -> Ref (rec depth ty) $ if depth <= n then adj n else n
             Bind Pi ty name inType outType -> Bind Pi ty name (rec depth inType) (rec (1+depth) outType)
             Bind Lambda ty name inType body -> Bind Lambda (rec depth ty) name (rec depth inType) (rec (1+depth) body)
             Bind Fix ty name ty' body -> Bind Fix (rec depth ty) name (rec depth ty') (rec (1+depth) body)
             Apply ty function argument -> Apply (rec depth ty) (rec depth function) (rec depth argument)
             Hash ty name hash -> Hash (rec depth ty) name hash
   in rec 0

replace :: Expression -> Expression -> Expression
replace ref =
  let rec depth expr =
        case expr of
             Star _ -> expr
             Hole name -> Hole name
             Ref _ n -> if depth == n then shift (1 + depth +) ref else expr -- FIXME check type?
             Bind Pi ty name inType outType -> Bind Pi (rec depth ty) name (rec depth inType) (rec (1+depth) outType)
             Bind Lambda ty name inType body -> Bind Lambda (rec depth ty) name (rec depth inType) (rec (1+depth) body)
             Bind Fix ty name ty' body -> Bind Fix (rec depth ty) name (rec depth ty') (rec (1+depth) body)
             Apply ty function argument -> Apply (rec depth ty) (rec depth function) (rec depth argument)
             Hash ty name hash -> Hash (rec depth ty) name hash
   in shift (-1 +) . rec 0

reduce :: Expression -> Expression
reduce expr =
      case expr of
           Bind Pi ty name inType outType -> Bind Pi ty name (reduce inType) (reduce outType)
           Bind Lambda ty name inType body -> Bind Lambda ty name (reduce inType) (reduce body)
           Bind Fix ty ty' name body -> Bind Fix ty ty' name (reduce body)
           Apply ty function argument -> 
             let rf = reduce function
                 ra = reduce argument
              in case rf of
                      Bind Fix _ _ _ body -> reduce $ Apply ty (replace rf body) ra
                      Bind Lambda _ _ _ body -> reduce $ replace argument body
                      _ -> Apply ty ra rf
           _ -> expr

applicate :: (H.Hash -> Maybe Expression) -> Expression -> Expression
applicate hashExpr expr =
  case expr of
       Apply ty function argument ->
         let function' = applicate hashExpr function
          in case function' of
                  Bind Lambda _ _ _ body -> applicate hashExpr $ replace argument body
                  _ -> Apply ty function' argument
       Hash _ _ h ->
         maybe expr (applicate hashExpr) $ hashExpr h
       Bind Fix _ _ _ body ->
         applicate hashExpr $ replace expr body
       _ -> expr

-- are the two expressions equivalent
equiv :: (H.Hash -> Maybe Expression) -> Expression -> Expression -> Bool
equiv hashExpr =
  let eq x y =
        case (x,y) of
             (Star nX, Star nY) -> nX == nY
             (Hole _, Hole _) -> True
             (Ref _ nX, Ref _ nY) -> nX == nY
             (Bind Fix _ _ _ bX, Bind Fix _ _ _ bY) -> eq bX bY
             (Bind Pi _ _ inTypeX outTypeX, Bind Pi _ _ inTypeY outTypeY) ->
               eq inTypeX inTypeY && eq outTypeX outTypeY
             (Bind Lambda _ _ inTypeX bodyX, Bind Lambda _ _ inTypeY bodyY) ->
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
             (Bind Fix _ _ _ b, _) -> eq (replace x b) y
             (_, Bind Fix _ _ _ b) -> eq x (replace y b)
             (_, _) -> False

   in eq
