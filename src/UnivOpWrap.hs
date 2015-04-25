--------------------------------------------------------------------------------
-- |
-- Module      : UnivOpWrap
-- Note        :
--
--
--
--------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
module UnivOpWrap
  ( defaultRoutine, testRoutine, askRoutine
  , sanitizeMetaFromCommand
  ) where

import System.Directory
import System.Process
import System.IO
import UnivOpWrap.Meta
import UnivOpWrap.Logic
import UnivOpWrap.Helper
import Colors

routine :: String -> String -> (Meta -> IO()) -> IO[Meta]
routine c n action = do
  meta <- loadMeta c
  ex <- doesFileExist n
  if ex
    then do
      f <- cleanPath n
      action $ constM f
      return (updateMeta (constM f) meta)
    else do
      let mtch = findBestMatch (meta,n)
      if mtch /= Non
        then do
          action mtch
          return (updateMeta mtch meta)
        else return meta

testRoutine :: String -> [String] -> IO()
testRoutine c ns = do
  ms <- routine c (unwords ns) (\ m ->
    putStrLn $ yellowString c ++ " " ++ greenString (show m))
  showMetas ms
  saveMeta c ms

defaultRoutine :: String -> [String] -> IO()
defaultRoutine c ns = do
  ms <- routine c (unwords ns) (\ m -> do
    putStrLn $ yellowString c ++ " " ++ greenString (show m)
    ext <- system $ c ++ " \"" ++ fn m ++ "\""
    yellowPrint ext)
  saveMeta c ms

askRoutine :: String -> [String] -> IO()
askRoutine c ns = do
  ms <- routine c (unwords ns) (\ m -> do
    putStrLn $ yellowString c ++ " " ++ greenString (show m)
    putStr " [Y/n]: "
    hFlush stdout
    str <- getLine
    case str of
      "n" -> redPutStrLn "abort"
      _   -> do
        ext <- system $ c ++ " \"" ++ fn m ++ "\""
        yellowPrint ext)
  saveMeta c ms
