{-#OPTIONS_GHC -Wall #-}

data Expr
  = Ref Int
  | Lam Expr
  | App Expr Expr
--  deriving Show

instance Show Expr where
  show e = case e of
                Ref n   -> show n
                Lam x   -> "λ" ++ show x
                App f a -> "`" ++ show f ++ "," ++ show a

instance Eq Expr where
  (==) a b = case reduce a of
                  Ref n   -> case b' of
                                  Ref m -> n == m
                                  _     -> False
                  Lam f   -> case b' of
                                  Lam g -> f == g
                                  _     -> False
                  App f x -> case b' of
                                  App g y -> f == g && x == y
                                  _       -> False
    where
      b' = reduce b


rec :: (a -> a -> a) -> (Int -> Int -> a) -> (a -> a) -> Int -> Expr -> a
rec app ref lam = inner 
  where 
   inner cut e = case e of
                      Ref n -> n `ref` cut
                      App f a -> (inner cut f) `app` (inner cut a)
                      Lam f -> lam (inner (cut+1) f)

isClosed :: Expr -> Bool
isClosed = rec (&&) (<) id 0

etable :: Expr -> Bool
etable = rec (&&) (/=) id 0

shift :: Int -> Expr -> Expr
shift z = rec App (\n c -> if n >= c then Ref (n + z) else Ref n) Lam 0

replace :: Expr -> Expr -> Expr
replace a = rec App (\n c -> if n == c then shift c a else Ref n) Lam 0

reduce :: Expr -> Expr
reduce e = case e of
                App f a -> case reduce f of
                                Lam x -> reduce $ replace (reduce a) (reduce x) -- β-reduce
                                f' -> App f' (reduce a)
                Lam x -> case reduce x of
                              App f (Ref 0) -> if etable f
                                                  then reduce (shift (-1) f) -- η-reduce
                                                  else e
                              x' -> Lam x'
                _ -> e

