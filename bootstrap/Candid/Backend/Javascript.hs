module Candid.Backend.Javascript
  ( Javascript(..)
  ) where

import Candid.Expression
import Candid.Backend

data Javascript = JS (String -> String) (String -> String)

jscompile :: Expression -> Int -> String -> String
jscompile expr depth rest =
  case expr of
       Ref _ n -> "v" ++ show (depth - 1 - n) ++ rest
       Name _ _ body ->
         let depth' = if closed body then 0 else depth
             wrapper :: (String -> String) -> String -> String
             wrapper inner more = "(()=>{var v" ++ show depth' ++ "=" ++ inner (";return f" ++ show depth' ++ ";})()" ++ more)
          in wrapper (jscompile body (depth'+1)) rest
       Lambda _ _ _ body ->
         let depth' = if closed body then 0 else depth
             wrapper :: (String -> String) -> String -> String
             wrapper inner more = "((v" ++ show depth' ++ ")=>" ++ inner (")" ++ more)
          in wrapper (jscompile body (depth'+1)) rest
       Apply _ function argument ->
         jscompile function depth ('(':jscompile argument depth (')':rest))
       Hash _ _ hsh -> '$' : show hsh ++ rest
       _ -> "undefined" ++ rest

instance Backend Javascript
  where
    empty = JS id id
    exec (JS defs execs) expr = JS defs (\rest -> execs (jscompile expr 0 (";\n" ++ rest)))
    define (JS defs execs) hsh expr = JS (\rest -> defs ("var $" ++ show hsh ++ " = " ++ jscompile expr 0 (";\n" ++ rest))) execs
    render (JS defs execs) = defs $ execs ""

