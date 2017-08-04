{-#OPTIONS_GHC -Wall #-}

module Candid.TypeCheck
  ( TypeError(..) 
  , typeOf
  ) where

import Candid.Expr
import Candid.Hash
import Data.Map

data TypeError
  = UntypedBox
  | OpenExpression [Expr] Expr
  | InvalidInputType Expr
  | InvalidOutputType Expr
  | NotAFunction Expr Expr
  | TypeMismatch Expr Expr Expr
  | HashNotFound Hash
  deriving (Eq, Show)

index :: Word -> [a] -> Maybe a
index _ []       = Nothing
index 0 (x : _)  = Just x
index n (_ : xs) = index (n-1) xs

-- type check an expression in a context
typeIn :: Map Hash (Expr, Expr) -> [Expr] -> Expr -> Either TypeError Expr
typeIn hm = ti
  where
    ti ctx e = case e of
                    -- The type of a reference is the same as the input type to the
                    -- lamba it refers to. Unbound references are type errors.
                    Ref n   -> case index n ctx of
                                    Nothing -> Left $ OpenExpression ctx (Ref n)
                                    Just t -> Right t
                    -- Box has no type.
                    Box -> Left UntypedBox
                    -- Star has the type of Box.
                    Star -> Right Box
                    -- Remarks are merely that.
                    Rem _ x -> ti ctx x
                    -- Type Annotation must match!
                    TA t f -> loftE (redux $ ti ctx f) $
                      \r -> if r == s
                               then Right r
                               else Left $ TypeMismatch e s r
                         where s = reduce t
                    -- A lambda (that checks) has an input and output type joined by Pi.
                    -- The input type is simply t.
                    -- The output type is the type of it's body.
                    Lam t f -> loftE (ti ctx t) $
                      \_ -> loftE (ti (ctx `with` t) f) $
                        Right . Pi t
                    -- Function application *must* take a lambda or a reference to one,
                    -- which have Pi types, and an argument that matches the input type.
                    -- It's resulting type is the output type of that lambda,
                    -- with any references to that lambda replaced.
                    App f a -> loftE (redux $ unhax $ ti ctx f) $
                      \x -> case x of
                                 Pi s t -> loftE (ti ctx a) $
                                   \r -> if r == s
                                            then Right $ replace a t
                                            else Left $ TypeMismatch e s r
                                 r      -> Left $ NotAFunction f r
                    -- Both input and output types of a function must indeed be types,
                    -- resolving to Star (or Box, I guess).
                    Pi t f  -> loftE (redux $ ti ctx t) $
                      \x -> case x of
                                 Star -> right
                                 Box  -> right
                                 _    -> Left $ InvalidInputType t
                        where
                          right = loftE (redux $ ti (ctx `with` t) f) $
                            \x' -> case x' of
                                        Star -> Right Star
                                        Box  -> Right Box
                                        _       -> Left $ InvalidOutputType x'
                    -- Look up the hash and extract it's type.
                    Hash h -> case Data.Map.lookup h hm of
                                   Nothing     -> Left $ HashNotFound h
                                   Just (_, t) -> Right $ unhash hm t
    -- automatically propogate errors
    loftE :: Either a b -> (b -> Either a b) -> Either a b
    loftE (Left er) _ = Left er
    loftE (Right x) f = f x
    -- open up a potentially hashed expression
    unhax :: Either a Expr -> Either a Expr
    unhax x = loftE x $ Right . unhash hm
    -- reduce propogating erorrs
    redux :: Either a Expr -> Either a Expr
    redux x = loftE x $ Right . reduce
    -- prepare a new context given another type on the stack
    with :: [Expr] -> Expr -> [Expr]
    with ctx' t = Prelude.map (shift 1) $ t : ctx'

-- Type check a closed expression.
typeOf :: Map Hash (Expr, Expr) -> Expr -> Either TypeError Expr
typeOf hm = typeIn hm []

