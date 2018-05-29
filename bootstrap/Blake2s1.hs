module Blake2s1
  ( Hash(..)
  , Salt
  , zero
  , hash
  , toList
  , fromList
  , toHex
  , fromHex
  ) where

import qualified Data.Hashable as Hashable
import qualified Numeric
import Data.Bits
import Data.Word

data Hash = H Word32 Word32 Word32 Word32 Word32 Word32 Word32 Word32
  deriving Eq

zero :: Hash
zero = H 0 0 0 0 0 0 0 0

type State = ( Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32 )
type Salt = ( Word32, Word32, Word32, Word32 )

(|>) :: a -> (a -> b) -> b
(|>) x f = f $! x

-- half of G
mix :: (Word32, Word32, Word32, Word32) -> Word32 -> Int -> Int -> (Word32, Word32, Word32, Word32)
mix (a,b,c,d) w rd rb =
  let a' = a + w + b
      d' = (d `xor` a') `rotateR` rd
      c' = c + d'
      b' = (b `xor` c') `rotateR` rb
   in (a',b',c',d')
{-# INLINE mix #-}

-- half of a bunch of G that don't use the same data
group :: State -> (Word32, Word32, Word32, Word32) -> Int -> Int -> State
group (a,b,c,d, e,f,g,h, i,j,k,l, m,n,o,p) (w,x,y,z) rd rb =
  let (a', b', c', d') = mix (a,b,c,d) w rd $! rb
      (e', f', g', h') = mix (e,f,g,h) x rd $! rb
      (i', j', k', l') = mix (i,j,k,l) y rd $! rb
      (m', n', o', p') = mix (m,n,o,p) z rd $! rb
   in (a',b',c',d', e',f',g',h', i',j',k',l', m',n',o',p')
{-# INLINE group #-}

-- A single round of the hash function
r :: State -> State -> State
r (m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,mA,mB,mC,mD,mE,mF)
      (v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,vA,vB,vC,vD,vE,vF) =
  let (w0,w4,w8,wC,w1,w5,w9,wD,w2,w6,wA,wE,w3,w7,wB,wF) =
        group (v0,v4,v8,vC,v1,v5,v9,vD,v2,v6,vA,vE,v3,v7,vB,vF) (m0,m2,m4,m6) 16 12
      (x0,x4,x8,xC,x1,x5,x9,xD,x2,x6,xA,xE,x3,x7,xB,xF) =
        group (w0,w4,w8,wC,w1,w5,w9,wD,w2,w6,wA,wE,w3,w7,wB,wF) (m1,m3,m5,m7) 8 7
      (y0,y5,yA,yF,y1,y6,yB,yC,y2,y7,y8,yD,y3,y4,y9,yE) =
        group (x0,x5,xA,xF,x1,x6,xB,xC,x2,x7,x8,xD,x3,x4,x9,xE) (m8,mA,mC,mE) 16 12
      (z0,z5,zA,zF,z1,z6,zB,zC,z2,z7,z8,zD,z3,z4,z9,zE) =
        group (y0,y5,yA,yF,y1,y6,yB,yC,y2,y7,y8,yD,y3,y4,y9,yE) (m9,mB,mD,mF) 8 7
   in (z0,z1,z2,z3,z4,z5,z6,z7,z8,z9,zA,zB,zC,zD,zE,zF)
{-# INLINE r #-}

hash :: Hash -> Hash -> Salt -> Hash
hash (H m0 m1 m2 m3 m4 m5 m6 m7) (H m8 m9 mA mB mC mD mE mF) (s0, s1, s2, s3) =
  let v0 = 0x6b08e647 -- 0x6a09e667 ^ 0x01010020, no key and digest length of 32 bytes
      v1 = 0xbb67ae85 -- no leaf length
      v2 = 0x3c6ef372 -- no node offset
      v3 = 0xa54ff53a -- no node offset, no node depth, no inner length
      v4 = 0x510e527f `xor` s0
      v5 = 0x9b05688c `xor` s1
      v6 = 0x1f83d9ab `xor` s2
      v7 = 0x5be0cd19 `xor` s3
      v8 = 0x6a09e667
      v9 = 0xbb67ae85
      vA = 0x3c6ef372
      vB = 0xa54ff53a
      vC = 0x510e523f -- 0x510e527f ^ 0x40,
      vD = 0x9b05688c
      vE = 0xe07c2654 -- ~0x1f83d9ab,
      vF = 0x5be0cd19

      -- the rounds
      (w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,wA,wB,wC,wD,wE,wF) =
        (v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,vA,vB,vC,vD,vE,vF) |>
        r (m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,mA,mB,mC,mD,mE,mF) |>
        r (mE,mA,m4,m8,m9,mF,mD,m6,m1,mC,m0,m2,mB,m7,m5,m3) |>
        r (mB,m8,mC,m0,m5,m2,mF,mD,mA,mE,m3,m6,m7,m1,m9,m4) |>
        r (m7,m9,m3,m1,mD,mC,mB,mE,m2,m6,m5,mA,m4,m0,mF,m8) |>
        r (m9,m0,m5,m7,m2,m4,mA,mF,mE,m1,mB,mC,m6,m8,m3,mD) |>
        r (m2,mC,m6,mA,m0,mB,m8,m3,m4,mD,m7,m5,mF,mE,m1,m9) |>
        r (mC,m5,m1,mF,mE,mD,m4,mA,m0,m7,m6,m3,m9,m2,m8,mB) |>
        r (mD,mB,m7,mE,mC,m1,m3,m9,m5,m0,mF,m4,m8,m6,m2,mA) |>
        r (m6,mF,mE,m9,mB,m3,m0,m8,mC,m2,mD,m7,m1,m4,mA,m5) |>
        r (mA,m2,m8,m4,m7,m6,m1,m5,mF,mB,m9,mE,m3,mC,mD,m0)

  in
    H (0x6b08e647 `xor` w0 `xor` w8)
      (0xbb67ae85 `xor` w1 `xor` w9)
      (0x3c6ef372 `xor` w2 `xor` wA)
      (0xa54ff53a `xor` w3 `xor` wB)
      (0x510e527f `xor` w4 `xor` wC `xor` s0)
      (0x9b05688c `xor` w5 `xor` wD `xor` s1)
      (0x1f83d9ab `xor` w6 `xor` wE `xor` s2)
      $! (0x5be0cd19 `xor` w7 `xor` wF `xor` s3)

byteSwap :: Word32 -> Word32
byteSwap x =
  (x `shiftR` 24) .|. ((x `shiftR` 8) .&. 0x0000ff00) .|. ((x `shiftL` 8) .&. 0x00ff0000) .|. (x `shiftL` 24)

pad :: Int -> a -> [a] -> [a]
pad n x xs = replicate (n - length xs) x ++ xs

hexWord :: Word32 -> String
hexWord n = pad 8 '0' $ Numeric.showHex n ""

hexRead :: String -> Word32
hexRead s = read $ '0':'x':s

toList :: Hash -> [Word32]
toList (H a b c d e f g h) = [a,b,c,d,e,f,g,h]

fromList :: [Word32] -> Maybe Hash
fromList [a,b,c,d,e,f,g,h] = Just (H a b c d e f g h)
fromList _ = Nothing

toHex :: Hash -> String
toHex h = concat $ map (hexWord . byteSwap) (toList h)

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)

fromHex :: String -> Maybe Hash
fromHex = fromList . map byteSwap . map hexRead . chop 8

instance Show Hash where
  showsPrec _ h = (toHex h ++)

instance Ord Hash where
  compare x y = compare (toList x) (toList y)

instance Hashable.Hashable Hash where
  hashWithSalt salt (H a b c d e f g h) = (fromIntegral a) + salt
