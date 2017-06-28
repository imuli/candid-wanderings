{-#OPTIONS_GHC -Wall #-}

import Data.Hashable

data Constant
  = Star
  | Box
  deriving Eq

instance Hashable Constant where
  hashWithSalt s c = case c of
                          Box  -> s `hashWithSalt` (0 :: Int)
                          Star -> s `hashWithSalt` (1 :: Int)

data Expr
  = Ref Int
  | Pi Expr Expr
  | Lam Expr Expr
  | App Expr Expr
  | Const Constant
  deriving (Eq)

data TypeError
  = UntypedBox
  | OpenExpression [Expr] Expr
  | InvalidInputType Expr
  | InvalidOutputType Expr
  | NotAFunction Expr
  | TypeMismatch Expr Expr
  deriving (Eq, Show)

index :: Int -> [a] -> Maybe a
index _ [] = Nothing
index 0 (x : _) = Just x
index n (_ : xs) = index (n-1) xs

instance Show Expr where
  show e = case e of
                Ref n   -> show n
                Lam t f -> "λ" ++ show t ++ " " ++ show f
                Pi t f  -> "π" ++ show t ++ " " ++ show f
                App f a -> "`" ++ show f ++ " " ++ show a
                Const Star -> "*"
                Const Box -> "□"

instance Hashable Expr where
  hashWithSalt s e = case e of
                          Ref n   -> s `hashWithSalt` (0 :: Int) `hashWithSalt` n
                          Const c -> s `hashWithSalt` (1 :: Int) `hashWithSalt` c
                          App f a -> s `hashWithSalt` (2 :: Int) `hashWithSalt` f `hashWithSalt` a
                          Pi  t f -> s `hashWithSalt` (3 :: Int) `hashWithSalt` t `hashWithSalt` f
                          Lam t f -> s `hashWithSalt` (4 :: Int) `hashWithSalt` t `hashWithSalt` f

typeIn :: [Expr] -> Expr -> Either TypeError Expr
typeIn ctx e = case e of
                    Ref n   -> case index n ctx of
                                    Nothing -> Left $ OpenExpression ctx (Ref n)
                                    Just t -> Right t
                    Const c -> case c of
                                    Box -> Left UntypedBox
                                    Star -> Right $ Const Box
                    Lam t f -> loftE (typeIn ctx t) $
                      \_ -> loftE (typeIn (ctx `with` t) f) $
                        Right . Pi t
                    App f a -> loftE (redux $ typeIn ctx f) $
                      \x -> case x of
                                 Pi s t -> loftE (typeIn ctx a) $
                                   \r -> if r == s
                                            then Right $ shift (-1) $ replace a t
                                            else Left $ TypeMismatch s r
                                 _      -> Left $ NotAFunction f
                    Pi t f  -> loftE (typeIn ctx t) $
                      \x -> case x of
                                 Const _ -> loftE (typeIn (ctx `with` t) f) $
                                   \x' -> case x' of
                                               Const r -> Right $ Const r
                                               _       -> Left $ InvalidOutputType x'
                                 _       -> Left $ InvalidInputType t
  where
    loftE :: Either a b -> (b -> Either a b) -> Either a b
    loftE (Left er) _ = Left er
    loftE (Right x) f = f x
    redux :: Either a Expr -> Either a Expr
    redux x = loftE x $ Right . reduce
    with :: [Expr] -> Expr -> [Expr]
    with ctx' t = map (shift 1) $ t : ctx'

rec :: (a -> a -> a) -> (Int -> Int -> a) -> (a -> a -> a) -> (a -> a -> a) -> (Constant -> a) -> Int -> Expr -> a
rec app ref lam pi_ con = inner 
  where 
   inner cut e = case e of
                      Ref n   -> n `ref` cut
                      App f a -> (inner cut f) `app` (inner cut a)
                      Lam t f -> (inner cut t) `lam` (inner (cut+1) f)
                      Pi t f  -> (inner cut t) `pi_` (inner (cut+1) f)
                      Const c -> con c

isClosed :: Expr -> Bool
isClosed = rec (&&) (<) (&&) (&&) (\_ -> True) 0

etable :: Expr -> Bool
etable = rec (&&) (/=) (&&) (&&) (\_ -> True) 0

shift :: Int -> Expr -> Expr
shift z = rec App (\n c -> if n >= c then Ref (n + z) else Ref n) Lam Pi Const 0

replace :: Expr -> Expr -> Expr
replace a = rec App (\n c -> if n == c then shift c a else Ref n) Lam Pi Const 0

reduce :: Expr -> Expr
reduce e = case e of
                Pi t x -> Pi (reduce t) (reduce x)
                App f a -> case reduce f of
                                Lam _ x -> reduce $ replace (reduce a) (reduce x) -- β-reduce
                                f' -> App f' (reduce a)
                Lam t x -> case reduce x of
                                App f (Ref 0) -> if etable f
                                                    then reduce (shift (-1) f) -- η-reduce
                                                    else e
                                x' -> Lam (reduce t) x'
                _ -> e

