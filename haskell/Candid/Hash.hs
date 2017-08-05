module Candid.Hash
  ( Hash(..)
  , Hashable(..)
  , prettyHash
  , readHash
  , nullHash
  ) where

import qualified Data.Vector as V
import qualified Numeric
import Data.Bits
import Data.Word
import Data.Binary
import Data.Char (ord)
import qualified Text.ParserCombinators.ReadP as RP

iv :: V.Vector Word32
iv = V.fromList [ 0x6a09e667
                , 0xbb67ae85
                , 0x3c6ef372
                , 0xa54ff53a
                , 0x510e527f
                , 0x9b05688c
                , 0x1f83d9ab
                , 0x5be0cd19
                ]

sigma :: V.Vector (V.Vector Int)
sigma = V.fromList $
  map V.fromList [ [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15]
                 , [ 14, 10,  4,  8,  9, 15, 13,  6,  1, 12,  0,  2, 11,  7,  5,  3]
                 , [ 11,  8, 12,  0,  5,  2, 15, 13, 10, 14,  3,  6,  7,  1,  9,  4]
                 , [  7,  9,  3,  1, 13, 12, 11, 14,  2,  6,  5, 10,  4,  0, 15,  8]
                 , [  9,  0,  5,  7,  2,  4, 10, 15, 14,  1, 11, 12,  6,  8,  3, 13]
                 , [  2, 12,  6, 10,  0, 11,  8,  3,  4, 13,  7,  5, 15, 14,  1,  9]
                 , [ 12,  5,  1, 15, 14, 13,  4, 10,  0,  7,  6,  3,  9,  2,  8, 11]
                 , [ 13, 11,  7, 14, 12,  1,  3,  9,  5,  0, 15,  4,  8,  6,  2, 10]
                 , [  6, 15, 14,  9, 11,  3,  0,  8, 12,  2, 13,  7,  1,  4, 10,  5]
                 , [ 10,  2,  8,  4,  7,  6,  1,  5, 15, 11,  9, 14,  3, 12, 13,  0]
                 ]

r = [ 16, 12,  8,  7 ]

mix :: Word32 -> Word32 -> [Word32] -> [Word32]
mix x y [a0, b0, c0, d0] =
  let a1 = a0 + b0 + x
      d1 = (d0 `xor` a1) `rotateR` 16
      c1 = c0 + d1
      b1 = (b0 `xor` c1) `rotateR` 12
      a2 = a1 + b1 + y
      d2 = (d1 `xor` a2) `rotateR` 8
      c2 = c1 + d2
      b2 = (b1 `xor` c2) `rotateR` 7
   in [a2, b2, c2, d2]


appV :: V.Vector a -> [Int] -> ([a] -> [a]) -> V.Vector a
appV v is f = v V.// (zip is $ f $ map (v V.!) is)

mixer :: V.Vector Word32 -> V.Vector Int -> V.Vector Word32 -> (Int, Int, [Int]) -> V.Vector Word32
mixer m s v (a, b, is) = appV v is (mix (ms a) (ms b))
  where
    ms x = m V.! (s V.! x)

mix_round :: V.Vector Word32 -> V.Vector Word32 -> V.Vector Int -> V.Vector Word32
mix_round m v s = foldl (mixer m s) v [ ( 0, 1, [0, 4, 8, 12])
                                      , ( 2, 3, [1, 5, 9, 13])
                                      , ( 4, 5, [2, 6,10, 14])
                                      , ( 6, 7, [3, 7,11, 15])
                                      , ( 8, 9, [0, 5,10, 15])
                                      , (10,11, [1, 6,11, 12])
                                      , (12,13, [2, 7, 8, 13])
                                      , (14,15, [3, 4, 9, 14])
                                      ]

(!/) :: V.Vector a -> (Int, (a -> a)) -> V.Vector a
(!/) v (i,f) = v V.// [(i, f $ v V.! i)]

initial :: V.Vector Word32 -- `h`     current hash state
        -> V.Vector Word32 -- The initial local vector for each compression.
initial h =
  (h V.++ iv) !/
    (12, xor 0x40) !/
      (14, xor 0xffffffff)

compress :: V.Vector Word32 -- `h`     current hash state
         -> V.Vector Word32 -- `m`     message to compress
         -> V.Vector Word32
compress h m =
  V.zipWith xor h $
    uncurry (V.zipWith xor) $ V.splitAt 8 $
      V.foldl (mix_round m) (initial h) sigma

padd :: Ord a => Num a => Bits a => (a, a) -> a -> (a, a)
padd (h, l) a = let nl = l + a
                    nh = h + if nl < l then 1 else 0
                 in (nh, nl)

initialState :: V.Vector Word32
initialState = iv !/ (0, xor $ 0x01010020)

newtype Hash = Blake2 (V.Vector Word32)
  deriving (Eq, Ord)

pad :: Int -> a -> [a] -> [a]
pad n x xs = replicate (n - length xs) x ++ xs

showHex :: Word32 -> String
showHex n = pad 8 '0' $ Numeric.showHex n ""

byteSwap :: Word32 -> Word32
byteSwap x =
  (x `shiftR` 24) .|. ((x `shiftR` 8) .&. 0x0000ff00) .|. ((x `shiftL` 8) .&. 0x00ff0000) .|. (x `shiftL` 24)

prettyHash :: Hash -> String
prettyHash (Blake2 h) = concat $ V.map (showHex . byteSwap) h

getHex :: RP.ReadP Word32
getHex = RP.choice [ (\c -> fromIntegral $ ord c - ord '0') <$> RP.satisfy (\c -> '0' <= c && c <= '9')
                   , (\c -> fromIntegral $ ord c - ord 'A' + 10) <$> RP.satisfy (\c -> 'A' <= c && c <= 'F')
                   , (\c -> fromIntegral $ ord c - ord 'a' + 10) <$> RP.satisfy (\c -> 'a' <= c && c <= 'f')
                   ]

getLEWord32 :: RP.ReadP Word32
getLEWord32 = byteSwap <$> foldl (\s a -> 16 * s + a) 0 <$> RP.count 8 getHex

readHash :: RP.ReadP Hash
readHash = Blake2 <$> V.fromList <$> RP.count 8 getLEWord32

instance Read Hash where
  readsPrec _ = RP.readP_to_S readHash

instance Show Hash where
  showsPrec _ h = (prettyHash h ++)

instance Binary Hash where
  put (Blake2 h) = put $ V.toList h
  get = Blake2 <$> V.fromList <$> get

class Hashable a where
  hashedWith :: a -> Hash -> Hash
  hashedWith x h = hash x `hashedWith` h
  hash :: a -> Hash
  hash = flip hashedWith nullHash

instance Hashable Hash where
  hashedWith (Blake2 msg) (Blake2 h) = Blake2 $ compress initialState $ msg V.++ h

instance Hashable Word64 where
  hashedWith msg = hashedWith $ Blake2 $ V.fromList $ low : high : replicate 6 0
    where
      high :: Word32
      high = fromIntegral $ msg `shiftR` 16
      low :: Word32
      low = fromIntegral $ msg

instance Hashable Word32 where
  hashedWith msg = hashedWith $ Blake2 $ V.fromList $ msg : replicate 7 0

instance Hashable Word16 where
  hashedWith msg = hashedWith $ Blake2 $ V.fromList $ (fromIntegral msg) : replicate 7 0

instance Hashable Word8 where
  hashedWith msg = hashedWith $ Blake2 $ V.fromList $ (fromIntegral msg) : replicate 7 0

instance Hashable Word where
  hashedWith msg = hashedWith (fromIntegral msg :: Word64)

nullHash :: Hash
nullHash = Blake2 $ V.fromList $ replicate 8 0

