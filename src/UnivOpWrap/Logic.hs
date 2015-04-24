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
  ( findMatches
  , findBestMatch
  , Meta (..)
  ) where

import UnivOpWrap.Meta
import Colors

newtype Matcher = MkMatcher { runMatcher :: [Meta] -> [Meta] }

matchChar :: Char -> Matcher
matchChar c = let
    matchCharM Non                  = Non
    matchCharM M{rs = []}           = Non
    matchCharM i | head (rs i) == c = i{rs = tail (rs i)
                                       ,hi = hi i ++ greenString [c]}
                 | otherwise        = matchCharM i{rs = tail (rs i)
                                                  ,hi = hi i ++ [head (rs i)]}
  in MkMatcher $ map matchCharM

matchString :: String -> Matcher
matchString []     = MkMatcher id
matchString (c:cs) = MkMatcher $
  runMatcher (matchString cs) . runMatcher (matchChar c)

findMatches :: ([Meta], String) -> [Meta]
findMatches (ms,n) = [i | i <- runMatcher (matchString n) ms
                        , i /= Non]

findBestMatch :: ([Meta], String) -> Meta
findBestMatch t = minimum (findMatches t)
