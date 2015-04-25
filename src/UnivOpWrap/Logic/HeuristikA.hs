{-# LANGUAGE CPP #-}
module UnivOpWrap.Logic.HeuristikA
  ( matchCharM, matchStringM, matchStringMs
  ) where

import System.HsTColors
import UnivOpWrap.Meta
import UnivOpWrap.Logic.Common

matchCharM :: Char -> Meta -> Meta
matchCharM _ Non                     = Non
matchCharM _ M{rs = []}              = Non
matchCharM c m@M{rs=r, hi=h} | head r == c = m{rs = tail r
                                              ,hi = h ++ greenString [c]}
                             | otherwise   = matchCharM c m{rs = tail r
                                                           ,hi = h ++ [head r]}

-- matchCharMs :: Char -> [Meta] -> [Meta]
-- matchCharMs c = map (matchCharM c)

matchStringM :: String -> Meta -> Meta
matchStringM []     = id
matchStringM (c:cs) = matchStringM cs . matchCharM c

matchStringMs :: String -> [Meta] -> [Meta]
#if 0
matchStringMs []     = id
matchStringMs (c:cs) = matchStringMs cs . matchCharMs c
#else
matchStringMs = matcherMap matchStringM
#endif
