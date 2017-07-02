{-#OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}

module HashExpr
  ( HashExpr(..)
  , enhash
  ) where

import GHC.Generics
import qualified Core as E
import Data.Map
import Data.Hashable

data HashExpr
  = Box
  | Star
  | Hash Int
  | Ref Int
  | App HashExpr HashExpr
  | Lam HashExpr HashExpr
  | Pi HashExpr HashExpr
  deriving (Eq, Generic)

instance Hashable HashExpr
instance Show HashExpr where
  show he = case he of
                Box     -> "□"
                Star    -> "*"
                Hash n  -> "#" ++ show n
                Ref n   -> show n
                App f a -> "`" ++ show f ++ " " ++ show a
                Lam t f -> "λ<" ++ show t ++ "> " ++ show f
                Pi t f  -> "π<" ++ show t ++ "> " ++ show f

enhash :: E.Expr -> Map Int HashExpr -> (HashExpr, Map Int HashExpr)
enhash e m = case E.typeOf e of
                  Right _ -> case e of
                                  E.Const E.Box  -> (Box  , m)
                                  E.Const E.Star -> (Star , m)
                                  E.Ref n    -> (Ref n, m)
                                  E.Lam t f  -> mkHash Lam t f
                                  E.Pi t f   -> mkHash Pi t f
                                  E.App f a  -> mkHash App f a
                  Left _  -> case e of
                                  E.Const E.Box  -> (Box  , m)
                                  E.Const E.Star -> (Star , m)
                                  E.Ref n    -> (Ref n, m)
                                  E.Lam t f  -> mkHE Lam t f
                                  E.Pi t f   -> mkHE Pi t f
                                  E.App f a  -> mkHE App f a
        where 
          mkHE :: (HashExpr -> HashExpr -> HashExpr) -> E.Expr -> E.Expr -> (HashExpr, Map Int HashExpr)
          mkHE f a b = let (a', m') = enhash a m
                           (b', m'') = enhash b m'
                        in
                           (f a' b', m'')
          mkHash :: (HashExpr -> HashExpr -> HashExpr) -> E.Expr -> E.Expr -> (HashExpr, Map Int HashExpr)
          mkHash f a b = let (he, m') = mkHE f a b
                             h        = hash he
                          in
                             (Hash h, insert h he m')

