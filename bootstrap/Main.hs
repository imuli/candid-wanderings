module Main where

import System.Environment
import System.Directory (renameFile)
import Control.Exception

import Candid.Store
import Candid.Parse
import Candid.Typecheck
import Candid.Backend
import Candid.Backend.Javascript

import Data.Maybe (listToMaybe, catMaybes)
import Data.List (intercalate)

usage :: IO ()
usage = do
  putStrLn "commands:"
  putStrLn "\tshow (name)+\tshow entries with name"
  putStrLn "\tload (file)\tload new expressions from file"
  putStrLn "\tcompile (exprs)\tcompile expressions"

nullFile :: IOException -> IO String
nullFile _ = return ""

loadStore :: IO Store
loadStore = do
  content <- (readFile "candid.store") `catch` nullFile
  return $ case listToMaybe $ reads content of
                Nothing -> Candid.Store.empty
                Just (store, _) -> store

saveStore :: Store -> IO ()
saveStore store = do
  writeFile "candid.store.new" (show store)
  renameFile "candid.store.new" "candid.store"

loadFile :: String -> Store -> IO Store
loadFile fname store = do
  input <- readFile fname `catch` nullFile
  case fmap catMaybes $ parseText input of
       Left err -> do putStrLn $ show err
                      return store
       Right [] -> return store
       Right exprs ->
         case numberInto store exprs of
              Left (at, err) -> do putStrLn $ "Error in " ++ at ++ "\n" ++ prettyError err
                                   return store
              Right st -> return st

main :: IO ()
main = do
  args <- getArgs
  store <- loadStore
  case listToMaybe args of
       Just "show" -> putStrLn $ unlines $ map (unlines . (map prettyEntry) . byName store) (tail args)
       Just "load" -> loadFile (head $ tail args) store >>= saveStore
       Just "compile" ->
         case fmap catMaybes $ parseText $ intercalate " " $ tail args of
              Left err -> putStrLn $ show err
              Right [] -> putStrLn "Nothing to compile."
              Right exprs -> putStrLn $ compile (Candid.Backend.empty :: Javascript) store $ map (fillHoles store) exprs
       _ -> usage
