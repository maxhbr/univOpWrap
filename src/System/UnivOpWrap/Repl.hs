--------------------------------------------------------------------------------
-- |
-- Module      : UnivOpWrap.Repl
-- Note        :
--
-- partly stolen from https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Building_a_REPL
--
--------------------------------------------------------------------------------
module System.UnivOpWrap.Repl
    (runRepl
    ) where

import System.IO
import Data.List (nub)
import Data.Either
import Control.Monad
import System.HsTColors
import System.UnivOpWrap.Common
import System.UnivOpWrap.Logic

--------------------------------------------------------------------------------
promptText = "univOpWrap>>> " ++ show ANSIRed
numOfShowedItms = 9
maxLength = 80

--------------------------------------------------------------------------------

prompt ms = let
    printBest :: [MData] -> IO()
    printBest ms = let
        shortenString s = let
                       lengthDiffWithoutColor = length (uncolor s) - maxLength
                  in if lengthDiffWithoutColor < 0
                     then s
                     else "..." ++ drop lengthDiffWithoutColor s
        notNonM = nub $ takeOnlyNotNon ms
        prettyPrint (nr, Non _) = putStrLn $ "[" ++ show nr ++ "] Non"
        prettyPrint (nr, m)     = putStrLn $
          "[" ++ show nr ++ "] " ++ (shortenString $ pr1 m ++ pr2 m)
      in do
        mapM_ prettyPrint $ zip [1..] $ take numOfShowedItms notNonM
        when (length notNonM > numOfShowedItms) $ putStrLn "[...]"

    readPrompt :: String -> IO String
    readPrompt prompt = let
        flushStr :: String -> IO ()
        flushStr str = putStr str >> hFlush stdout
      in do
        flushStr prompt
        result <- getLine
        print ANSINone
        return result
  in printBest ms >> readPrompt promptText

evalAndUpdate :: [MData] -> String -> IO [MData]
evalAndUpdate ms expr = let
    evalString :: String -> IO (Either String Int)
    evalString (' ':s) = evalString s
    evalString s       = case reads s :: [(Int, String)] of
                           [(i, "")] -> return $ Right i
                           _         -> return $ Left s
  in do
    result <- evalString expr
    return $ case result of
      Left ""   -> ms
      Left str  -> takeOnlyNotNon $ findMatches str ms
      Right int -> [nub ms !! (int -1)]

loop :: [MData] -> IO MData
loop [] = error "no more possible matches :("
loop ms = let
    nubMs = nub ms
  in if length nubMs == 1
    then return $ head nubMs
    else prompt nubMs >>= evalAndUpdate ms >>= loop

runRepl :: String -> Info -> IO MData
runRepl arg i = let
    ms = if null arg
           then md i
           else takeOnlyNotNon $ findMatches arg (md i)
  in do
    unless (null arg) (putStrLn $ promptText ++ arg ++ show ANSINone)
    loop ms
