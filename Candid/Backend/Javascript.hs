module Candid.Backend.Javascript
  ( Javascript(..)
  ) where

import Candid.Expression
import Candid.Backend
import Data.Map.Lazy as M
import qualified Blake2s1 as H

data Javascript = JS (String -> String) (String -> String)

funcs :: Map H.Hash (Bool, String)
funcs = fromList $ Prelude.map (\(h,v) -> (maybe H.zero id $ H.fromHex h, v)) $
  [ ("dec7f572ba56cc8a4ebe0bb3e5fd2f27cf227c5a6279a71e4151204729d46369", -- Boolean
    (False, "(b)=>(t)=>(f)=>b?t:f"))
  , ("c9790141e68367bfb74be063c4f8253d0d4eb1f81fd6b277296eef3b08d9ae07", -- True
    (True, "true"))
  , ("3b48fbd01cc7b4180bba723534847d7df89325cf0fbfde749d7e78e0401d5a98", -- False
    (True, "false"))
  , ("0f243dfb4aae2dcac1e2a75284395fdf726a07ca44f2d998710116ad4333576c", -- not
    (False, "(x)=>!x"))
  , ("78b812d7cee780e143d250489884bf6bc05e9d2e7767f40e6a953b107430e720", -- and
    (False, "(x)=>(y)=>x&&y"))
  , ("1ebd3cc4bfc75e9e0250497fc322cdf86fd378405e01cec3e0d40be063c06764", -- or
    (False, "(x)=>(y)=>x||y"))
  , ("40377fc44a862222b2b025a7c2251e801367cf1dd074eee3eb72beb300b162da", -- xor
    (False, "(x)=>(y)=>x!=y"))
  , ("f65bc2e7bc8c10ea1344bb305dea1bc13d21a28c23bdc5a413c6ff9e6b4cb87d", -- eqBoolean
    (False, "(x)=>(y)=>(i)=>x==y"))
  , ("71cc540ffe43544c325bbdbf931d0b91630095e8feefb201042f9a5599544f9f", -- Natural
    (False, "(n)=>(zero)=>(succ)=>n===0?zero:succ(n-1)"))
  , ("a23d794e6afcd0e8f066a628ea336e83170fb47f68aaedac8923749bc7aff38e", -- Zero
    (True, "0"))
  , ("4d6fc413d381589048d5ad6535027aa7899fae0a691a1cfaa70144957f8af7dc", -- Succ
    (False, "(n)=>1+n"))
  , ("d3c1ebf6ea09dde459bf4d24e1f4cd6c65e9e8bd3ced15e0b22b270535d83f3c", -- addNatural
    (False, "(m)=>(n)=>m+n"))
  , ("d1d43362b5599376e3f95f515c11194074a7366bfa820473da96955502f56513", -- mulNatural
    (False, "(m)=>(n)=>m*n"))
  , ("6e44f829df385522a372133316185d8803a2852da563d78e9c43ceed4bac77c6", -- eqNatural
    (False, "(m)=>(n)=>(i)=>m==n"))
  , ("a1ca8ad11d6a94a3f85c2ab507d3e1d46c8de6228a8e8b49d144e77bf2436234", -- ordNatural
    (False, "(m)=>(n)=>(i)=>m<n?$2deb44f8a3a2f75e5bdbeb9ff7bde37c992c9259af6aa61a885a2acd76affd34:m==n?e31afbe771ccae915838134a5e6395c7e912e4b049a1e41738599ab83fc4fccf:de0cd43c8a7a07c49e82eb537d429479ddc09adb5b76c358088ac6c2ae5a4c1e"))
  , ("362ab0b562f153548c722a18768096c74265f8118ae696e38d86af9b2cf876ff", -- List t
    (False, "(xs)=>(nil)=>(cons)=>xs===[]?nil:cons(xs[0])(xs.slice(1))"))
  , ("6ec6f4be87fdf1d4231bfecc01a78a7aa942db99bffa61cd26077d8b1167bcd2", -- Nil
    (True, "[]"))
  , ("cd3f4ed6dcda4dafa035da30228a0a45618af8b030219de06b1a9601234426eb", -- Cons
    (False, "(x)=>(xs)=>[x,...xs]"))
  ]

-- find the highest order type
-- which is to say, turn:
--   List x into List
--   Pair a b into Pair
--   ...
root :: Expression -> H.Hash
root expr =
  case expr of
       Apply _ function _ -> root function
       Hash _ _ h -> h
       _ -> hashOf expr

jscompile :: Expression -> Int -> String -> String
jscompile expr depth rest =
  case M.lookup (hashOf expr) funcs of
       Just (inline, value) ->
         (if inline then value else '$':show (hashOf expr)) ++ rest
       Nothing ->
         case stripTypes expr of
              Star _ -> "undefined" ++ rest
              Hole _ -> "undefined" ++ rest
              Pi _ _ _ -> "undefined" ++ rest
              Ref _ n -> "v" ++ show (depth - 1 - n) ++ rest
              Name _ name body -> "/* " ++ name ++ " */" ++
                if closed body
                   then jscompile body depth rest
                   else let wrapper :: (String -> String) -> String -> String
                            wrapper inner more = "(()=>{var v" ++ show depth ++ "=" ++ inner (";return v" ++ show depth ++ ";})()" ++ more)
                         in wrapper (jscompile body (depth+1)) rest
              Lambda _ _ _ body ->
                let depth' = if closed body then 0 else depth
                    wrapper :: (String -> String) -> String -> String
                    wrapper inner more = "(v" ++ show depth' ++ ")=>" ++ inner (more)
                 in wrapper (jscompile body (depth'+1)) rest
              Apply _ function argument ->
                let h = root $ typeOf function
                 in case M.lookup h funcs of
                         Nothing -> jscompile function depth ("("++jscompile argument depth (")"++rest))
                         Just (False,_) -> '$':show h++"("++jscompile function depth (")("++jscompile argument depth (")"++rest))
                         Just (True,value) -> value++"("++jscompile function depth (")("++jscompile argument depth (")"++rest))
              Hash _ _ h -> '$':show h ++ rest

instance Backend Javascript
  where
    empty = JS id id
    exec (JS defs execs) expr = JS defs (\rest -> execs ("console.log(" ++ jscompile expr 0 (");\n" ++ rest)))
    define (JS defs execs) hsh expr =
      let defs' =
            case M.lookup hsh funcs of
                 Nothing -> \rest -> defs ("var $" ++ show hsh ++ " = " ++ jscompile expr 0 (";\n" ++ rest))
                 Just (True,_) -> defs
                 Just (False,value) -> \rest -> defs ("var $" ++ show hsh ++ " = " ++ value ++ (";\n" ++ rest))
       in JS defs' execs
    render (JS defs execs) = "'use_strict';\n" ++ (defs $ execs "")

