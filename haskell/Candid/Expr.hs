{-#OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Candid.Expr
  ( Expr(..)
  , TypeError(..)
  , typeOf
  , reduce
  , enhash
  , unhash
  , hashExpr
  , pretty
  , readExpr
  , readExprL
  , isBoxy
  , hashify
  ) where

import Candid.Hash
import Data.Map
import Data.Binary
import qualified Text.ParserCombinators.ReadP as RP

data Expr
  = Ref Word
  | Pi Expr Expr
  | Lam Expr Expr
  | App Expr Expr
  | Hash Hash
  | Star
  | Box
  | Rem String Expr
  | TA Expr Expr
  deriving (Show, Read)

instance Eq Expr where
  (==) Star      Star      = True
  (==) Box       Box       = True
  (==) (Ref n)   (Ref m)   = n == m
  (==) (Pi t f)  (Pi s g)  = t == s && f == g
  (==) (Lam t f) (Lam s g) = t == s && f == g
  (==) (App f a) (App g b) = a == b && f == g
  (==) (Rem _ x) e         = x == e
  (==) e         (Rem _ x) = x == e
  (==) (TA  _ x) e         = x == e
  (==) e         (TA  _ x) = x == e
  (==) (Hash h)  (Hash k)  = h == k
  (==) (Hash h)  e         = h == hash' e
  (==) e         (Hash h)  = h == hash' e
  (==) _         _         = False

getStar :: RP.ReadP Expr
getStar = RP.char '*' *> return Star

getBox :: RP.ReadP Expr
getBox = RP.char '□' *> return Box

getBoxEOF :: RP.ReadP Expr
getBoxEOF = RP.eof *> return Box

digit :: RP.ReadP Char
digit = RP.satisfy $ \c -> '0' <= c && c <= '9'

getRef :: RP.ReadP Expr
getRef = Ref <$> read <$> RP.many1 digit

getPi :: RP.ReadP Expr
getPi = RP.char 'π' *> (Pi <$> readExpr <*> readExpr)

getLam :: RP.ReadP Expr
getLam = RP.char 'λ' *> (Lam <$> readExpr <*> readExpr)

getApp :: RP.ReadP Expr
getApp = RP.char '$' *> (Lam <$> readExpr <*> readExpr)

getRem :: RP.ReadP Expr
getRem = RP.string "-- " *> (Rem <$> RP.munch (/= '\n') <*> readExpr)

getType :: RP.ReadP Expr
getType = RP.char ':' *> (TA <$> readExpr <*> readExpr)

getHash :: RP.ReadP Expr
getHash = RP.char '#' *> (Hash <$> readHash)

readExpr :: RP.ReadP Expr
readExpr = RP.skipSpaces *> RP.choice [ getStar
                                      , getBox
                                      , getRef
                                      , getPi
                                      , getLam
                                      , getApp
                                      , getRem
                                      , getType
                                      , getHash
                                      , getBoxEOF
                                      ]

readExprL :: RP.ReadP [Expr]
readExprL = RP.sepBy readExpr (RP.optional $ RP.char '\n')

instance Binary Expr where
  put Star      = put (248 :: Word8)
  put (Pi t f)  = put (249 :: Word8) >> put t >> put f
  put (Lam t f) = put (250 :: Word8) >> put t >> put f
  put (App f a) = put (251 :: Word8) >> put f >> put a
  put (Hash h)  = put (252 :: Word8) >> put h
  put (Rem n x) = put (253 :: Word8) >> put n >> put x
  -- skip type annotations in binary format
  put (TA _ x)  = put x
  put Box       = fail "Tried to serialize □."
  put (Ref n)   = if n < 248
                     then put ((fromIntegral n) :: Word8)
                     else if n < 65536 + 248
                             then put (255 :: Word8) >>
                               put (fromIntegral (n - 248) :: Word16)
                             else fail "Reference is way too big."
  get = do x <- get :: Get Word8
           case x of
                248 -> return Star
                249 -> Pi <$> get <*> get
                250 -> Lam <$> get <*> get
                251 -> App <$> get <*> get
                252 -> Hash <$> get
                253 -> Rem <$> get <*> get
                254 -> reserved x
                255 -> Ref . ((+ 248) . fromIntegral :: Word16 -> Word) <$> get
                n -> return $ Ref $ fromIntegral n

reserved :: Monad m => Word8 -> m a
reserved x = fail $ "Byte " ++ show x ++ " is reserved."

pretty' :: Int -> Int -> Expr -> String
pretty' i j e = replicate i ' ' ++
  case e of
       Ref n   -> show n
       Lam t f -> "λ" ++ pretty' 1 j' t ++ "\n" ++ pretty' j j' f
       Pi t f  -> "π" ++ pretty' 1 j' t ++ "\n" ++ pretty' j j' f
       TA t f  -> ":" ++ pretty' 1 j' t ++ "\n" ++ pretty' j j' f
       App f a -> "$" ++ pretty' 1 j' f ++ "\n" ++ pretty' j j' a
       Rem n x -> "-- " ++ n ++ "\n" ++ pretty' i j x
       Star    -> "*"
       Box     -> "□"
       Hash h  -> "#" ++ prettyHash h
  where
    j' = j + 2

pretty :: Expr -> String
pretty = pretty' 0 2

instance Hashable Expr where
  hashedWith (Hash h)  = hashedWith h
  hashedWith Star      = hashedWith (248 :: Word8)
  hashedWith (Pi t f)  = hashedWith (249 :: Word8) . hashedWith t . hashedWith f
  hashedWith (Lam t f) = hashedWith (250 :: Word8) . hashedWith t . hashedWith f
  hashedWith (App f a) = hashedWith (251 :: Word8) . hashedWith f . hashedWith a
  hashedWith (Rem _ x) = hashedWith x
  hashedWith (TA _ x)  = hashedWith x
  hashedWith Box       = hashedWith (254 :: Word8)
  hashedWith (Ref n)   = hashedWith (255 :: Word8) . hashedWith n

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

hashExpr :: Expr -> Expr
hashExpr = Hash . hash'

hash' :: Expr -> Hash
hash' e = case e of
               Hash h -> h
               _      -> hash e

hashify :: Map Hash (Expr, Expr) -> Expr -> Expr
hashify mh e = case e of
                    App t f -> ify $ App (hashify mh t) (hashify mh f)
                    Lam f a -> ify $ Lam (hashify mh f) (hashify mh a)
                    Pi f a  -> ify $ Pi (hashify mh f) (hashify mh a)
                    Rem s x -> Rem s $ hashify mh x
                    _ -> e
  where
    ify x = let h = hash' x
             in case Data.Map.lookup h mh of
                     Nothing -> x
                     Just _  -> Hash h

enhash :: Map Hash (Expr, Expr) -> Expr -> (Map Hash (Expr, Expr), Expr)
enhash mh0 e = case e of
                    App t f -> mkHash App t f
                    Lam f a -> mkHash Lam f a
                    Pi f a  -> mkHash Pi f a
                    Rem s x -> let (mh1, x') = enhash mh0 x
                                in (mh1, Rem s x')
                    _       -> (mh0, e)
  where
    mkHash :: (Expr -> Expr -> Expr) -> Expr -> Expr -> (Map Hash (Expr, Expr), Expr)
    mkHash kind a b =
      let (mh1, a') = enhash mh0 a
          (mh2, b') = enhash mh1 b
          e'        = kind a' b'
       in case typeOf mh2 e' of
               Left _  -> (mh2, e')
               Right t -> let (mh3, t') = enhash mh2 t
                              h         = hash' e'
                           in (insert h (e', t') mh3, Hash h)

unhash :: Map Hash (Expr, Expr) -> Expr -> Expr
unhash hm e = rec App (\n _ -> Ref n) Lam Pi Star Box hsh 0 e
  where
    hsh :: Hash -> Expr
    hsh h = case Data.Map.lookup h hm of
                   Nothing -> Hash h
                   Just (x, _) -> unhash hm x

-- Type check a closed expression.
typeOf :: Map Hash (Expr, Expr) -> Expr -> Either TypeError Expr
typeOf hm = typeIn hm []

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

--isClosed :: Expr -> Bool
--isClosed = rec (&&) (<) (&&) (&&) (\_ -> True) 0

-- check for an instance of (Ref 0) in an expression
-- if there isn't any we can η-reduce
etable :: Expr -> Bool
etable = rec (&&) (/=) (&&) (&&) True True (\_ -> True) 0

-- check for boxes
isBoxy :: Expr -> Bool
isBoxy = rec (||) (\_ _ -> False) (||) (||) False True (\_ -> False) 0

-- adjust instances of (Ref 0) and above by some amount
shift :: Word -> Expr -> Expr
shift z = rec App (\n c -> if n >= c then Ref (n + z) else Ref n) Lam Pi Star Box Hash 0

-- replace instances of (Ref 0)
replace :: Expr -> Expr -> Expr
replace a = shift (-1) . rec App (\n c -> if n == c then shift (c+1) a else Ref n) Lam Pi Star Box Hash 0

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
