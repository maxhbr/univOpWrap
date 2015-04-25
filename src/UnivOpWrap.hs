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
  ( defaultRoutine, forkRoutine, testRoutine, askRoutine
  , sanitizeMetaFromCommand
  ) where

import System.Directory
import System.Process
import System.IO
import UnivOpWrap.Meta
import UnivOpWrap.Logic
import UnivOpWrap.Helper
import Systom.HsTColors

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

defaultRoutine :: String -> [String] -> IO()
defaultRoutine c ns = do
  ms <- routine c (unwords ns) (\ m -> do
    putStrLn $ yellowString c ++ " " ++ greenString (show m)
#if 1
    ext <- system $ c ++ " \"" ++ fn m ++ "\""
#else
    p <- runCommand $ c ++ " \"" ++ fn m ++ "\""
    ext <- waitForProcess p
#endif
    yellowPrint ext)
  saveMeta c ms

forkRoutine :: String -> [String] -> IO()
forkRoutine c ns = do
  ms <- routine c (unwords ns) (\ m -> do
    _ <- runCommand $ c ++ " \"" ++ fn m ++ "\""
    putStrLn $ yellowString c ++ " " ++ greenString (show m))
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

testRoutine :: String -> [String] -> IO()
testRoutine c ns = do
  ms <- routine c (unwords ns) (\ m ->
    putStrLn $ yellowString c ++ " " ++ greenString (show m))
  showMetas ms
  saveMeta c ms
