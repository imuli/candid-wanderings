{-#OPTIONS_GHC -Wall #-}

module Candid.Reduce
  ( reduce
  ) where

import Candid.Expr
import Candid.Util

-- check for an instance of (Ref 0) in an expression
-- if there isn't any we can η-reduce
etable :: Expr -> Bool
etable = i 0
  where
    i :: Word -> Expr -> Bool
    i c e = case e of
                 Ref n   -> n /= c
                 App f a -> (i c f) && (i c a)
                 Lam t f -> (i c t) && (i (c+1) f)
                 Pi t f  -> (i c t) && (i (c+1) f)
                 Rem _ x -> i c x
                 TA  _ x -> i c x
                 _ -> True

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
                -- reduction removes remarks
                Rem _ x -> reduce x
                -- reduction removes type annotation
                TA _ f -> reduce f
                _ -> e
