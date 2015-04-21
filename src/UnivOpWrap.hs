module UnivOpWrap
  ( defaultRoutine
  , sanitizeMetaFromCommand
  ) where

import Data.List

import Colors

import Debug.Trace

defaultRoutine :: String -> [String] -> IO()
defaultRoutine c ns = do
  meta <- getMetaFromCommand c
  trace c (findBestMatch (meta,ns))

-- |Obtains the 'meta-info' corresponding to some command
getMetaFromCommand :: String -> IO [String]
getMetaFromCommand _ = return
  [ "uiae eai uaie"
  , "uaieuiaeuiae"
  , "123456789" ]

-- |Delets unused / notexistend files
sanitizeMetaFromCommand :: String -> IO ()
sanitizeMetaFromCommand = undefined

-- |Finds the best match
findBestMatch :: ([String], [String]) -> IO ()
findBestMatch (ms,ns) = let 
    matches = [m | m <- ms , and [n `isInfixOf` m | n <- ns]]
  in if not (null matches) 
    then putStrLn $ head matches
    else do
      redPrint   "No match found"
      putStrLn $ "The needles were: " ++ unwords ns
      putStrLn $ unlines matches
