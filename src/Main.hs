#!/usr/bin/env runhaskell

import System.Environment
import System.Exit
import Data.Maybe

import System.UnivOpWrap
import System.HsTColors

progName :: String
progName = "univOpWrap"
usage, version :: IO ()
usage   = putStrLn $ unlines
  [ "Usage: " ++ progName ++ " [-r] [-f] [-a] [-t] cmd [-l] [-h] [arg [arg [ ...]]]"
  , "   -r : run as repl (if parameter not uniqly determine file)"
  , "   -f : fork the process"
  , "   -a : ask, wether to run the command on the found file"
  , "   -t : activate the TUI (not implemented yet)"
  , "   -l : list all files corresponding to a command"
  , "   -s : sanitize the information / forget nonexisting files"
  , "   -h : show this text"
  , ""
  , "Written by: Maximilian Huber (mail@maximilian-huber.de)" ]
version = putStrLn $ progName ++ " 0.1.0.0"

main :: IO ()
main = let
    parse :: Parameter -> [String] -> IO ()
    parse _ ("-h":_)                        = usage
    parse _ ("-v":_)                        = version
    -- parse ["-s",c]                       = sanitizeMetaFromCommand c
    parse p ("-r":args)                     = parse p{repl=True} args
    parse p ("-f":args)                     = parse p{fork=True} args
    parse p ("-l":args)                     = parse p{list=True} args
    parse p ("-a":args)                     = parse p{ask=True} args
    parse p ("-d":args)                     = parse p{dbg=True} args
    parse p ("-t":args)                     = parse p{tui=True} args
    parse p ("-s":args)                     = parse p{sanitize=True} args
    parse p (a:args)    | isNothing (cmP p) = parse p{cmP=Just (commandFromString a)} args
                        | otherwise         = parse p{argsP=argsP p ++ [a]} args
    parse p []          | isNothing (cmP p) = redPrint "No command specified" >> exitWith (ExitFailure 1)
                        | otherwise         = univOpWrap p
  in
    getArgs >>= parse defaultParameter >> exitSuccess
