module Main where

import System.Environment

import Candid.Expression
import Candid.Store
import Candid.Parse

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
        let st = numberInto empty exprs
         in putStrLn $ unlines $ map (show . expr) $ byName st nm
