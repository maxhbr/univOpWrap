#!/usr/bin/env runhaskell

import System.Environment
import System.Exit
import Data.Maybe

import UnivOpWrap
import UnivOpWrap.Common
import System.HsTColors

progName :: String
progName = "univOpWrap"
usage, version :: IO ()
usage   = putStrLn $ "Usage: " ++ progName ++ " command [needle ..]"
version = putStrLn $ progName ++ " 0.1.0.0"

main :: IO ()
main = let
    parse :: Parameter -> [String] -> IO ()
    parse _ ("-h":_)    = usage
    parse _ ("-v":_)    = version
    -- parse ["-s",c]   = sanitizeMetaFromCommand c
    parse p ("-f":args) = parse p{fork=True} args
    parse p ("-l":args) = parse p{list=True} args
    parse p ("-a":args) = parse p{ask=True} args
    parse p (a:args)    | isNothing (cmP p) = parse p{cmP=Just (C a)} args
                        | otherwise         = parse p{argsP=argsP p ++ [a]} args
    parse p []          | isNothing (cmP p) = redPrint "No command specified" >> exitWith (ExitFailure 1)
                        | otherwise         = univOpWrap p
  in
    getArgs >>= parse defaultParameter >> exitSuccess
