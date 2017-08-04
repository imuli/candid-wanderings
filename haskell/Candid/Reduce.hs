{-#OPTIONS_GHC -Wall #-}

module Candid.Reduce
  ( reduce
  ) where

import Candid.Expr
import Candid.Hash

-- general form used by the following helpers
rec :: (a -> a -> a) -> (Word -> Word -> a) -> (a -> a -> a) -> (a -> a -> a) -> a -> a -> (Hash -> a) -> Word -> Expr -> a
rec app ref lam pi_ sta box hsh = inner
  where
   inner cut e = case e of
                      Ref n   -> n `ref` cut
                      App f a -> (inner cut f) `app` (inner cut a)
                      Lam t f -> (inner cut t) `lam` (inner (cut+1) f)
                      Pi t f  -> (inner cut t) `pi_` (inner (cut+1) f)
                      Rem _ x -> inner cut x
                      TA _ x  -> inner cut x
                      Star    -> sta
                      Box     -> box
                      Hash h  -> hsh h

-- check for an instance of (Ref 0) in an expression
-- if there isn't any we can η-reduce
etable :: Expr -> Bool
etable = rec (&&) (/=) (&&) (&&) True True (\_ -> True) 0

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
