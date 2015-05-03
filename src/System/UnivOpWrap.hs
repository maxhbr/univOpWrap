--------------------------------------------------------------------------------
-- |
-- Module      : UnivOpWrap
-- Note        :
--
--
--
--------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
module System.UnivOpWrap
  ( univOpWrap
  , module X
  ) where

import System.Directory
import System.Process
import System.FilePath
import System.IO
import System.HsTColors
import Control.Monad (when,unless,liftM)
import Data.Foldable (forM_)
import Data.Maybe

import System.UnivOpWrap.Common as X
import System.UnivOpWrap.Backend
import System.UnivOpWrap.Logic
import System.UnivOpWrap.Tui

univOpWrap :: Parameter -> IO()
univOpWrap p = do
    i <- getInfo (fromJust (cmP p))
    case p of
      p@P{list = True} -> mapM_ print $ md i
      _                -> do
        (i',ph) <- if tui p && null (argsP p)
                     then tuiRoutine (ask p) i
                     else defaultRoutine (ask p) (unwords (argsP p)) i
        _ <- when (dbg p) (print i')
        saveInfo i'
        unless (fork p) $ forM_ ph waitForProcess

defaultRoutine :: Bool -> String -> Info -> IO (Info, Maybe ProcessHandle)
defaultRoutine b arg i = do
    ex <- doesFileExist arg
    mtch <- if ex
      then do 
        cp <- cleanPath arg
        newMData cp 100
      else return $ findBestMatchI arg i
    case mtch of
      Non _ -> return (i, Nothing)
      _     -> do
        ph <- runCmd b (cm i) mtch
        return (updateInfo mtch i, ph)

tuiRoutine :: Bool -> Info -> IO(Info, Maybe ProcessHandle)
tuiRoutine _ i = do
    _ <- runTui [] i
    return undefined

runCmd :: Bool -> Command -> MData -> IO(Maybe ProcessHandle)
runCmd b c m = let
    -- l = show c ++ " \"" ++ fn m ++ "\""
    getCmdPerFT :: Command -> String -> String
    getCmdPerFT (C []) _               = "echo \"no cmd found for:\"; echo"
    getCmdPerFT (C (("",c):css)) ext'  = c
    getCmdPerFT (C ((ext,c):css)) ext' = if ext == ext'
      then c
      else getCmdPerFT (C css) ext
  in do
    let l = getCmdPerFT c (takeExtension (fn m))
         ++ " \"" ++ fn m ++ "\"" 
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
