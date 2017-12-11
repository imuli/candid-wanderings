module Blake2s1 exposing (Blake2s1, hash, toHex, toUni, zero)

import Native.Blake2s1

type Blake2s1 = B2 (List Int)

{-| hashes a two hashes and 4 Int salt into an hash -}
hash : Blake2s1 -> Blake2s1 -> Int -> Int -> Int -> Int -> Blake2s1
hash = Native.Blake2s1.hash

{-| renders a little endian hexstring -}
toHex : Blake2s1 -> String
toHex = Native.Blake2s1.toHex

{-| renders a little endian unicode string -}
toUni : Blake2s1 -> String
toUni = Native.Blake2s1.toUni

{-| a zero value for hashing tree leaves -}
zero : Blake2s1
zero = Native.Blake2s1.zero

