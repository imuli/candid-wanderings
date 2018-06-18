module Candid.Backend.Javascript
  ( Javascript(..)
  ) where

import Candid.Expression
import Candid.Backend

data Javascript = JS (String -> String) (String -> String)

showIntegral :: Integral t => t -> String
showIntegral n = show $ fromIntegral n

jscompile :: Integral t => Expression t -> t -> String -> String
jscompile expr depth rest =
  case expr of
       Ref n -> "v" ++ showIntegral (depth - 1 - n) ++ rest
       Rec n -> "f" ++ showIntegral (depth - 1 - n) ++ rest
       Lambda _ _ _ body ->
         let depth' = if closed body then 0 else depth
             recWrapper :: (String -> String) -> String -> String
             recWrapper s more =
               if hasRec body
                  then "(()=>{var f" ++ showIntegral depth' ++ "=" ++ s (";return f" ++ showIntegral depth' ++ ";})()" ++ more)
                  else "(" ++ s (")" ++ more)
             compBody :: String -> String
             compBody more = "(v" ++ showIntegral depth' ++ ")=>" ++ jscompile body (depth'+1) more
          in recWrapper compBody rest
       Apply _ function argument ->
         jscompile function depth ('(':jscompile argument depth (')':rest))
       Assert _ _ body -> jscompile body depth rest
       Hash _ hsh -> '$' : show hsh ++ rest
       _ -> "undefined" ++ rest

instance Backend Javascript
  where
    empty = JS id id
    exec (JS defs execs) expr = JS defs (\rest -> execs (jscompile expr 0 (";\n" ++ rest)))
    define (JS defs execs) hsh expr = JS (\rest -> defs ("var $" ++ show hsh ++ " = " ++ jscompile expr 0 (";\n" ++ rest))) execs
    render (JS defs execs) = defs $ execs ""

