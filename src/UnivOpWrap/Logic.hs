--------------------------------------------------------------------------------
-- | 
-- Module      : UnivOpWrap.Logic
-- Note        : 
-- 
-- 
-- 
--------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
module UnivOpWrap.Logic
  ( findBestMatch
  ) where

import Colors

-- |Checks, if a file is a match
isMatch :: [String] -> String -> Bool
isMatch ns m = let 
    isMatch' [] _                      = True
    isMatch' _ []                      = False
    isMatch' (a:as) (b:bs) | a == b    = isMatch' as bs
                           | otherwise = isMatch' (a:as) bs
  in
#if 1
    isMatch' (concat ns) m
#else
    and [n `isMatch` m | n <- ns]
#endif

-- |Finds the best match or start interactive search
findBestMatch :: ([String], [String]) -> IO ()
findBestMatch (ms,[]) = putStrLn $ "no search strings (length ms = " 
                     ++ yellowShow (length ms) ++ ")"
findBestMatch (ms,ns) = let 
    matches = [m | m <- ms , ns `isMatch` m]
  in if not (null matches) 
    then putStrLn $ head matches
    else do
      redPrint   "No match found"
      putStrLn $ "The needles were: " ++ unwords ns
      putStrLn $ unlines matches
