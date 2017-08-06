{-#OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Candid.Expr
  ( Expr(..)
  , pretty
  , readExpr
  , readExprL
  ) where

import Control.Applicative
import Candid.Hash
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
  (==) (Hash h)  e         = h == hash e
  (==) e         (Hash h)  = h == hash e
  (==) _         _         = False

getStar :: RP.ReadP Expr
getStar = RP.char '*' *> return Star

getBox :: RP.ReadP Expr
getBox = RP.char '□' *> return Box

digit :: RP.ReadP Char
digit = RP.satisfy $ \c -> '0' <= c && c <= '9'

getRef :: RP.ReadP Expr
getRef = Ref <$> read <$> RP.many1 digit

getPi :: RP.ReadP Expr
getPi = (RP.char 'π' <|> RP.char 'x') *> (Pi <$> readExpr <*> readExpr)

getLam :: RP.ReadP Expr
getLam = (RP.char 'λ' <|> RP.char '\\') *> (Lam <$> readExpr <*> readExpr)

getApp :: RP.ReadP Expr
getApp = (RP.char '$' <|> RP.char '`') *> (App <$> readExpr <*> readExpr)

getRem :: RP.ReadP Expr
getRem = RP.string "-- " *> (Rem <$> RP.munch (/= '\n') <*> readExpr) <|>
         (flip Rem) Box <$> (RP.char '(' *> RP.munch (/= ')') <* RP.char ')' )

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
       Rem n Box -> "(" ++ n ++ ")"
       Rem n x -> "-- " ++ n ++ "\n" ++ pretty' i j x
       Star    -> "*"
       Box     -> "□"
       Hash h  -> "#" ++ prettyHash h
  where
    j' = j + 2

pretty :: Expr -> String
pretty = pretty' 0 2

instance Hashable Expr where
  hash (Hash h)  = h
  hash Star      = (248 :: Word8) `hashedWith` nullHash
  hash (Pi t f)  = (249 :: Word8) `hashedWith` (hash t `hashedWith` hash f)
  hash (Lam t f) = (250 :: Word8) `hashedWith` (hash t `hashedWith` hash f)
  hash (App f a) = (251 :: Word8) `hashedWith` (hash f `hashedWith` hash a)
  hash (Rem _ x) = hash x
  hash (TA _ x)  = hash x
  hash Box       = (254 :: Word8) `hashedWith` nullHash
  hash (Ref n)   = (255 :: Word8) `hashedWith` (n `hashedWith` nullHash)
