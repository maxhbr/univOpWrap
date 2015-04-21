#!/usr/bin/env runhaskell

import System.Environment   
import System.Exit

progName, usageString, versionString :: String
progName = "univOpWrap"
usageString = "Usage: " ++ progName ++ " command [needle ..]"
versionString = progName ++ " 0.1.0.0"
 
main :: IO ()
main = let
    usage   = putStrLn usageString
    version = putStrLn versionString
    exit    = exitSuccess
    die     = exitWith (ExitFailure 1)

    parse ["-h"] = usage   >> exit
    parse ["-v"] = version >> exit
    parse []     = die
    parse (c:ns) = return (getMetaFromCommand c ,ns)
  in
    getArgs >>= parse >>= findBestMatch

-- |Obtains the 'meta-info' corresponding to some command
getMetaFromCommand :: String -> [String]
getMetaFromCommand _ = []

-- |Finds the best match
findBestMatch :: ([String], [String]) -> IO ()
findBestMatch (_,ns) = do
  putStrLn   "No match found"
  putStrLn $ "The needles were: " ++ unwords ns
