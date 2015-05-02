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
import Control.Monad (when,unless,liftM)
import Data.Foldable (forM_)
import Data.Maybe

import UnivOpWrap.Common
import UnivOpWrap.Backend
import UnivOpWrap.Logic

univOpWrap :: Parameter -> IO()
univOpWrap p = do
    i <- getInfo (fromJust (cmP p))
    case p of
      p@P{list = True} -> mapM_ print $ md i
      _                -> do
        (i',ph) <- defaultRoutine (ask p) (unwords (argsP p)) i
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
