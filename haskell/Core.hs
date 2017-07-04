{-#OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Core
  ( Constant(..)
  , Expr(..)
  , TypeError(..)
  , typeOf
  , reduce
  , enhash
  , unhash
  , pretty
  ) where

import qualified Numeric (showHex)
import GHC.Generics
import Data.Hashable
import Data.Map

data Constant
  = Star
  | Box
  deriving (Eq, Generic)

instance Hashable Constant
instance Show Constant where
  show Star = "*"
  show Box  = "□"

newtype Hash = Value Word
  deriving (Eq, Ord)

pad :: Int -> a -> [a] -> [a]
pad n x xs = replicate (n - length xs) x ++ xs

instance Hashable Hash where
  hashWithSalt salt (Value n) = hashWithSalt salt n
instance Show Hash where
  show (Value n) = "(Value 0x" ++ (pad 16 '0' $ Numeric.showHex n "") ++ ")"

data Expr
  = Ref Int
  | Pi Expr Expr
  | Lam Expr Expr
  | App Expr Expr
  | Const Constant
  | Hash Hash Hash
  deriving (Eq, Generic, Show)

pretty' :: Int -> Int -> Expr -> String
pretty' i j e = replicate i ' ' ++
  case e of
       Ref n   -> show n
       Lam t f -> "λ" ++ pretty' 1 j' t ++ "\n" ++ pretty' j j' f
       Pi t f  -> "π" ++ pretty' 1 j' t ++ "\n" ++ pretty' j j' f
       App f a -> "$" ++ pretty' 1 j' f ++ "\n" ++ pretty' j j' a
       Const c -> show c
       Hash h t-> "#" ++ show h ++ show t
  where
    j' = j + 2

pretty :: Expr -> String
pretty = pretty' 0 2

instance Hashable Expr

data TypeError
  = UntypedBox
  | OpenExpression [Expr] Expr
  | InvalidInputType Expr
  | InvalidOutputType Expr
  | NotAFunction Expr Expr
  | TypeMismatch Expr Expr
  | HashNotFound Int
  deriving (Eq, Show)

index :: Int -> [a] -> Maybe a
index _ [] = Nothing
index 0 (x : _) = Just x
index n (_ : xs) = index (n-1) xs

-- type check an expression in a context
typeIn :: [Expr] -> Expr -> Either TypeError Expr
typeIn ctx e = case e of
                    -- The type of a reference is the same as the input type to the
                    -- lamba it refers to. Unbound references are type errors.
                    Ref n   -> case index n ctx of
                                    Nothing -> Left $ OpenExpression ctx (Ref n)
                                    Just t -> Right t
                    -- Box has no type.
                    -- Star has the type of Box.
                    Const c -> case c of
                                    Box -> Left UntypedBox
                                    Star -> Right $ Const Box
                    -- A lambda (that checks) has an input and output type joined by Pi.
                    -- The input type is simply t.
                    -- The output type is the type of it's body.
                    Lam t f -> loftE (typeIn ctx t) $
                      \_ -> loftE (typeIn (ctx `with` t) f) $
                        Right . Pi t
                    -- Function application *must* take a lambda or a reference to one,
                    -- which have Pi types, and an argument that matches the input type.
                    -- It's resulting type is the output type of that lambda,
                    -- with any references to that lambda replaced.
                    App f a -> loftE (redux $ typeIn ctx f) $
                      \x -> case x of
                                 Pi s t -> loftE (typeIn ctx a) $
                                   \r -> if hashExpr r == hashExpr s
                                            then Right $ replace a t
                                            else Left $ TypeMismatch s r
                                 r      -> Left $ NotAFunction f r
                    -- Both input and output types of a function must indeed be types,
                    -- resolving to Star (or Box, I guess).
                    -- TODO Not sure why we return the output type -
                    -- I guess the type of the input type might be Box...
                    Pi t f  -> loftE (redux $ typeIn ctx t) $
                      \x -> case x of
                                 Const _ -> loftE (redux $ typeIn (ctx `with` t) f) $
                                   \x' -> case x' of
                                               Const r -> Right $ Const r
                                               _       -> Left $ InvalidOutputType x'
                                 _       -> Left $ InvalidInputType t
                    -- 839657738154607712 is the hash of Const Star
                    Hash _ (Value 0x0ba710439f9d0460) -> Right $ Const Star
                    -- As we don't make Hashes of ill-typed expressions,
                    -- the type of t will always be Star.
                    Hash _ t -> Right $ Hash t (Value 0x0ba710439f9d0460)
  where
    -- automatically propogate errors
    loftE :: Either a b -> (b -> Either a b) -> Either a b
    loftE (Left er) _ = Left er
    loftE (Right x) f = f x
    -- reduce propogating erorrs
    redux :: Either a Expr -> Either a Expr
    redux x = loftE x $ Right . reduce
    -- prepare a new context given another type on the stack
    with :: [Expr] -> Expr -> [Expr]
    with ctx' t = Prelude.map (shift 1) $ t : ctx'

hashExpr :: Expr -> Expr
hashExpr = snd . enhash empty

hash' :: Expr -> Hash
hash' e = case e of
               Hash h _ -> h
               _        -> Value $ fromIntegral (hash e)

enhash :: Map Hash (Expr, Expr) -> Expr -> (Map Hash (Expr, Expr), Expr)
enhash mh0 e = case e of
                    App t f -> mkHash App t f
                    Lam f a -> mkHash Lam f a
                    Pi f a  -> mkHash Pi f a
                    _       -> (mh0, e)
  where
    mkHash :: (Expr -> Expr -> Expr) -> Expr -> Expr -> (Map Hash (Expr, Expr), Expr)
    mkHash kind a b = case typeOf e of
                           Left _  -> (mh0, e)
                           Right t -> let (mh1, a') = enhash mh0 a
                                          (mh2, b') = enhash mh1 b
                                          (mh3, t') = enhash mh2 t
                                          e'        = kind a' b'
                                          h         = hash' e'
                                       in (insert h (e', t') mh3, Hash h (hash' $ t'))

unhash :: Map Hash (Expr, Expr) -> Expr -> Expr
unhash hm e = rec App (\n _ -> Ref n) Lam Pi Const hsh 0 e
  where
    hsh :: Hash -> Hash -> Expr
    hsh h t = case Data.Map.lookup h hm of
                   Nothing -> Hash h t
                   Just (x, _) -> unhash hm x

-- Type check a closed expression.
typeOf :: Expr -> Either TypeError Expr
typeOf = typeIn []

-- general form used by the following helpers
rec :: (a -> a -> a) -> (Int -> Int -> a) -> (a -> a -> a) -> (a -> a -> a) -> (Constant -> a) -> (Hash -> Hash -> a) -> Int -> Expr -> a
rec app ref lam pi_ con hsh = inner
  where
   inner cut e = case e of
                      Ref n   -> n `ref` cut
                      App f a -> (inner cut f) `app` (inner cut a)
                      Lam t f -> (inner cut t) `lam` (inner (cut+1) f)
                      Pi t f  -> (inner cut t) `pi_` (inner (cut+1) f)
                      Const c -> con c
                      Hash h t-> hsh h t

--isClosed :: Expr -> Bool
--isClosed = rec (&&) (<) (&&) (&&) (\_ -> True) 0

-- check for an instance of (Ref 0) in an expression
-- if there isn't any we can η-reduce
etable :: Expr -> Bool
etable = rec (&&) (/=) (&&) (&&) (\_ -> True) (\_ _ -> True) 0

-- adjust instances of (Ref 0) and above by some amount
shift :: Int -> Expr -> Expr
shift z = rec App (\n c -> if n >= c then Ref (n + z) else Ref n) Lam Pi Const Hash 0

-- replace instances of (Ref 0)
replace :: Expr -> Expr -> Expr
replace a = shift (-1) . rec App (\n c -> if n == c then shift (c+1) a else Ref n) Lam Pi Const Hash 0

-- beta and eta reduce an expression
reduce :: Expr -> Expr
reduce e = case e of
                -- TODO is there some analogy of η-reduction here?
                Pi t x -> Pi (reduce t) (reduce x)
                -- if we can reduce f to a lambda, we can carry out the application
                App f a -> case reduce f of
                                Lam _ x -> reduce $ replace (reduce a) (reduce x) -- β-reduce
                                f' -> App f' (reduce a)
                -- if we can reduce x to a lambda and
                Lam t x -> case reduce x of
                                App f (Ref 0) -> if etable f
                                                    then reduce (shift (-1) f) -- η-reduce
                                                    else Lam (reduce t) $ App (reduce f) (Ref 0)
                                x' -> Lam (reduce t) x'
                _ -> e

