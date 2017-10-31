{-#OPTIONS_GHC -Wall #-}

module Main (main) where

import System.Environment
import Candid.Core
import Text.ParserCombinators.ReadP
import Data.Maybe (mapMaybe)

exprString :: String -> Expr
exprString s = case readP_to_S readExpr s of
                    []          -> Box
                    (e, "") : _ -> e
                    (_, _)  : _ -> Box

deannotate :: Expr -> Expr
deannotate e = case e of
                    Rem s x -> Rem s (deannotate x)
                    TA _ x -> deannotate x
                    _ -> e

annotateWith :: Expr -> Expr -> Expr
annotateWith t e = case e of
                        Rem s x -> Rem s (annotateWith t x)
                        TA _ x -> annotateWith t x
                        _ -> TA t e

maybeRemark :: Hash -> Expr -> Expr
maybeRemark h e = case e of
                       Rem s _ -> if s == hs
                                     then e
                                     else Rem hs e
                       _ -> Rem hs e
  where
    hs = ('#' : show h)

detitle :: Expr -> Expr
detitle e = case e of
                 Rem _ x -> detitle x
                 TA t x -> TA (detitle t) (detitle x)
                 _ -> e

cleanup :: Expr -> Expr
cleanup e = detitle $ deannotate e

annotate :: Store -> Expr -> Expr
annotate st e = case typeOf st e of
                     Left err -> Rem ("!!" ++ show err) e
                     Right t  -> annotateWith (hashWith st t) e

lazyTypeOf :: Store -> Expr -> Either TypeError Expr
lazyTypeOf st e = case e of
                       Rem _ x -> lazyTypeOf st x
                       TA t _  -> Right t
                       _       -> typeOf st e

element :: Eq a => [a] -> a -> Maybe Word
element []       _ = Nothing
element (x : xs) v = if v == x
                        then Just 0
                        else (1 +) <$> element xs v

dename :: Store -> Expr -> Expr
dename st = i []
  where
    i c e = case e of
                 App f a   -> App (i c f) (i c a)
                 Lam t f   -> Lam (i c t) (i ("" : c) f)
                 Pi t f    -> Pi (i c t) (i ("" : c) f)
                 Rem s Box -> case element c s of
                                   Nothing -> maybe e entryExpr $ findName s st
                                   Just n -> Ref n
                 Rem s (Lam t f) -> Rem s $ Lam (i c t) (i (s : c) f)
                 Rem s (Pi t f) -> Rem s $ Pi (i c t) (i (s : c) f)
                 Rem s x   -> Rem s $ i c x
                 TA t f    -> TA (i c t) (i c f)
                 _         -> e

index :: [a] -> Word -> Maybe a
index []       _ = Nothing
index (x : _)  0 = Just x
index (_ : xs) n = index xs (n-1)

enname :: Store -> Expr -> Expr
enname st = i []
  where
    i c e = case e of
                 App f a   -> App (i c f) (i c a)
                 Lam t f   -> Lam (i c t) (i ("" : c) f)
                 Pi t f    -> Pi (i c t) (i ("" : c) f)
                 Rem s (Lam t f) -> Rem s $ Lam (i c t) (i (s : c) f)
                 Rem s (Pi t f) -> Rem s $ Pi (i c t) (i (s : c) f)
                 Rem s x   -> Rem s $ i c x
                 TA t f    -> TA (i c t) (i c f)
                 Ref n     -> case index c n of
                                   Nothing -> e
                                   Just "" -> e
                                   Just s -> Rem s Box
                 Hash _    -> case find e st of
                                   Nothing -> e
                                   Just entry -> case entryName entry of
                                                      "" -> e
                                                      n -> Rem n Box
                 _         -> e

type Transform = (Store, Expr) -> (Store, Expr)

annotateT :: Transform
annotateT (st, e) = (st, annotate st e)

deannotateT :: Transform
deannotateT (st, e) = (st, deannotate e)

compressT :: Transform
compressT (st, e) = (st, hashWith st e)

expandT :: Transform
expandT (st, e) = (st, unhash st e)

checkT :: Transform
checkT (st, e) = case typeOf st e of
                      Left er -> error $ show er ++ " in expression " ++ show e
                      Right _ -> (st, e)

storeT :: Transform
storeT (st, e) = case lazyTypeOf st e of
                      Left _ -> (st, e)
                      Right t -> (add (name e) (cleanup e) t st, e)
  where
    name x = case x of
                Rem ('#' : _) x' -> name x'
                Rem s _ -> s
                _ -> ""

nameT :: Transform
nameT (st, e) = (st, enname st e)

denameT :: Transform
denameT (st, e) = (st, dename st e)

reduceT :: Transform
reduceT (st, e) = (st, reduce e)

remarkT :: Transform
remarkT (st, e) = (st, maybeRemark (hash e) e)

transforms :: [(String, Transform)]
transforms = [ ("annotate", annotateT)
             , ("check", checkT)
             , ("compress", compressT)
             , ("deannotate", deannotateT)
             , ("dename", denameT)
             , ("expand", expandT)
             , ("name", nameT)
             , ("reduce", reduceT)
             , ("remark", remarkT)
             , ("store", storeT)
             ]

transformExpr :: [(Transform)] -> Transform
transformExpr trans ste = foldl (flip ($)) ste trans

getTransforms :: [String] -> [Transform]
getTransforms names = mapMaybe ((flip lookup) transforms) names

defaultTs :: [Transform]
defaultTs = getTransforms [ "dename"
                          , "compress"
                          , "store"
                          , "remark"
                          , "annotate"
                          ]

main :: IO ()
main = do
  args <- getArgs
  input <- getContents
  loop (whichTs args) empty input
    where
      whichTs :: [String] -> [Transform]
      whichTs a = case a of
                       [] -> defaultTs
                       _ -> getTransforms a
      loop :: [Transform] -> Store -> String -> IO ()
      loop trans st i = 
        case readP_to_S (readExpr <* skipSpaces) i of
             [] -> fail "Parsing Error"
             ((e, i') : _) -> let (st', e') = transformExpr trans (st, e)
                               in do _ <- (putStrLn . pretty) e'
                                     case i' of
                                          [] -> return ()
                                          _ -> loop trans st' i'

