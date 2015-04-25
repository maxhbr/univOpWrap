#!/usr/bin/env runhaskell

import System.Environment   
import System.Exit

import UnivOpWrap
import System.HsTColors

progName :: String
progName = "univOpWrap"
usage, version :: IO ()
usage   = putStrLn $ "Usage: " ++ progName ++ " command [needle ..]"
version = putStrLn $ progName ++ " 0.1.0.0"
 
main :: IO ()
main = let
    parse :: [String] -> IO ()
    parse ["-h"]        = usage
    parse ["-v"]        = version
    parse []            = redPrint "No command specified" >> exitWith (ExitFailure 1)
    parse ["-s",c]      = sanitizeMetaFromCommand c
    parse ("-f":(c:ns)) = forkRoutine c ns
    parse ("-t":(c:ns)) = testRoutine c ns
    parse ("-a":(c:ns)) = askRoutine c ns
    parse (c:ns)        = defaultRoutine c ns
  in
    getArgs >>= parse >> exitSuccess
