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
      Left error -> putStrLn $ show error
      Right exprs -> 
        case numberInto empty exprs of
             Left error -> putStrLn $ show error
             Right st -> putStrLn $ unlines $ map show $ byName st nm
