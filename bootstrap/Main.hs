module Main where

import System.Environment

import Candid.Expression
import Candid.Store
import Candid.Parse
import Candid.Typecheck

import Data.Maybe (catMaybes)

main :: IO ()
main = do
  args <- getArgs
  input <- readFile (head args)
  let nm = head (tail args)
      parsed = parseText input
   in case fmap catMaybes parsed of
      Left err -> putStrLn $ show err
      Right exprs -> 
        case numberInto empty exprs of
             Left (at, err) -> putStrLn $ "Error in " ++ at ++ "\n" ++ prettyError err
             Right st -> putStrLn $ unlines $ map prettyEntry $ byName st nm
