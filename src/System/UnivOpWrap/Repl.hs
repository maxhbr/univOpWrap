--------------------------------------------------------------------------------
-- |
-- Module      : UnivOpWrap.Repl
-- Note        :
--
-- partly stolen from https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Building_a_REPL
--
--------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
module System.UnivOpWrap.Repl
    (runRepl
    ) where

import System.IO
import Data.List (nub)
import Data.Either
import Control.Monad
import System.UnivOpWrap.Common
import System.UnivOpWrap.Logic

--------------------------------------------------------------------------------
promptText = "univOpWrap>>> "
numOfShowedItms = 9

--------------------------------------------------------------------------------

prompt ms = let
    printBest :: [MData] -> IO()
    printBest ms = let
        notNonM = nub [m | m <- ms, isNotNon m]
        prettyPrint (nr, Non _) = putStrLn $ "[" ++ show nr ++ "] Non"
        prettyPrint (nr, m)     = putStrLn $
          "[" ++ show nr ++ "] " ++ pr1 m ++ pr2 m
      in do
        mapM_ prettyPrint $ zip [1..] $ take numOfShowedItms notNonM
        when (length notNonM > numOfShowedItms) $ putStrLn "[...]"

    readPrompt :: String -> IO String
    readPrompt prompt = let
        flushStr :: String -> IO ()
        flushStr str = putStr str >> hFlush stdout
      in flushStr prompt >> getLine
  in printBest ms >> readPrompt promptText

evalAndUpdate :: [MData] -> String -> IO [MData]
evalAndUpdate ms expr = let
    evalString :: String -> IO (Either String Int)
    evalString s = case reads s :: [(Int, String)] of
                     [(i, "")] -> return $ Right i
                     _         -> return $ Left s
  in do
    result <- evalString expr
    return $ case result of
      Left str  -> [m | m <- findMatches str ms, isNotNon m]
      Right int -> [nub ms !! (int -1)]

loop :: [MData]
     -> IO MData
loop ms = let
    nubMs = nub ms
    endCondition :: String  -- Input
                 -> [MData] -- current data
                 -> Bool
    endCondition "1" _ = True
    endCondition _ [m] = True
    endCondition _ _   = False
  in do
    result <- prompt nubMs
    if endCondition result nubMs
      then return $ head nubMs
      else evalAndUpdate ms result >>= loop

runRepl :: String -> Info -> IO MData
runRepl arg i = let
    ms = if null arg
           then md i
           else findMatches arg (md i)
  in do
    unless (null arg) (putStrLn $ promptText ++ arg)
    loop ms
