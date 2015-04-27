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
  ( univOpWrap
  ) where

import System.Directory
import System.Process
import System.IO
import System.HsTColors
import Control.Monad (when,liftM)
import Data.Foldable (forM_)

import UnivOpWrap.Common
import UnivOpWrap.Backend
import UnivOpWrap.Logic

univOpWrap :: String -> [String] -> IO()
univOpWrap c args = do
    (i,ph) <- getInfo (C c) >>= defaultRoutine (unwords args)
    saveInfo =<< showInfo i
    forM_ ph waitForProcess

showInfo :: Info -> IO Info
showInfo i = print i >> return i

defaultRoutine :: String -> Info -> IO (Info, Maybe ProcessHandle)
defaultRoutine arg i = do
    ex <- doesFileExist arg
    mtch <- if ex
      then newMData arg 100
      else return $ findBestMatch arg i
    case mtch of
      Non _ -> return (i, Nothing)
      _     -> do
        ph <- runCmd True (cm i) mtch
        return (updateInfo mtch i, ph)

runCmd :: Bool -> Command -> MData -> IO(Maybe ProcessHandle)
runCmd b c m = let
    l = show c ++ " \"" ++ fn m ++ "\""
  in do
    putStrLn l
    ans <- if b
      then do
        putStr " [Y/n]: "
        hFlush stdout
        getLine
      else return "y"
    case ans of
      "n" -> return Nothing
      _   -> liftM Just (runCommand l)

-- routine :: String -> String -> (Meta -> IO()) -> IO[Meta]
-- routine c n action = do
--   meta <- loadMeta c
--   ex <- doesFileExist n
--   if ex
--     then do
--       f <- cleanPath n
--       action $ constM f
--       return (updateMeta (constM f) meta)
--     else do
--       let mtch = findBestMatch (meta,n)
--       if mtch /= Non
--         then do
--           action mtch
--           return (updateMeta mtch meta)
--         else return meta

-- defaultRoutine :: String -> [String] -> IO()
-- defaultRoutine c ns = do
--   ms <- routine c (unwords ns) (\ m -> do
--     putStrLn $ yellowString c ++ " " ++ greenString (show m)
-- #if 1
--     ext <- system $ c ++ " \"" ++ fn m ++ "\""
-- #else
--     p <- runCommand $ c ++ " \"" ++ fn m ++ "\""
--     ext <- waitForProcess p
-- #endif
--     yellowPrint ext)
--   saveMeta c ms

-- forkRoutine :: String -> [String] -> IO()
-- forkRoutine c ns = do
--   ms <- routine c (unwords ns) (\ m -> do
--     _ <- runCommand $ c ++ " \"" ++ fn m ++ "\""
--     putStrLn $ yellowString c ++ " " ++ greenString (show m))
--   saveMeta c ms

-- askRoutine :: String -> [String] -> IO()
-- askRoutine c ns = do
--   ms <- routine c (unwords ns) (\ m -> do
--     putStrLn $ yellowString c ++ " " ++ greenString (show m)
--     putStr " [Y/n]: "
--     hFlush stdout
--     ans <- getLine
--     case ans of
--       "n" -> redPutStrLn "abort"
--       _   -> do
--         ext <- system $ c ++ " \"" ++ fn m ++ "\""
--         yellowPrint ext)
--   saveMeta c ms

-- testRoutine :: String -> [String] -> IO()
-- testRoutine c ns = do
--   ms <- routine c (unwords ns) (\ m ->
--     putStrLn $ yellowString c ++ " " ++ greenString (show m))
--   showMetas ms
--   saveMeta c ms
