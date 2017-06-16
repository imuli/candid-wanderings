{-#OPTIONS_GHC -Wall #-}

data Expr
  = Ref Int
  | Lam Expr
  | App Expr Expr
  deriving Show

shift :: Int -> Int -> Expr -> Expr
shift z cut e = case e of
                     Ref n -> if n >= cut
                                 then Ref (n + z)
                                 else e
                     App f a -> App (shift z cut f) (shift z cut a)
                     Lam f -> Lam (shift z (cut+1) f)

has :: Int -> Expr -> Bool
has cut e = case e of
                 Ref n -> if n == cut
                             then True
                             else False
                 App f a -> (has cut f) || (has cut a)
                 Lam f -> (has (cut+1) f)

replace :: Expr -> Int -> Expr -> Expr
replace a cut e = case e of
                       Ref n -> if n == cut
                                   then shift cut 0 a
                                   else e
                       App f aa -> App (replace a cut f) (replace a cut aa)
                       Lam f -> Lam $ replace a (cut+1) f

reduce :: Expr -> Expr
reduce e = case e of
                App f a -> case reduce f of
                                Lam x -> shift (-1) 1 $ reduce $ replace (reduce a) 0 x
                                f' -> App f' (reduce a)
                Lam f -> case reduce f of
                              App x (Ref 0) -> if has 0 x
                                                  then e
                                                  else reduce (shift (-1) 1 x) -- η-reduce
                              f' -> Lam f'
                _ -> e

