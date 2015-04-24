{-# LANGUAGE CPP #-}
module UnivOpWrap.Logic.HeuristikB
  ( matchCharM, matchStringM, matchStringMs
  ) where

import Data.List (isPrefixOf)
import Colors
import UnivOpWrap.Meta
import UnivOpWrap.Logic.Common

matchCharM :: Char -> Meta -> Meta
matchCharM c = matchStringM [c]

-- matchCharMs :: Char -> [Meta] -> [Meta]
-- matchCharMs c = map (matchCharM c)

matchStringM :: String -> Meta -> Meta
matchStringM _ Non             = Non
matchStringM [] m              = m
matchStringM _ M{rs = []}      = Non
matchStringM s m@M{rs=r, hi=h} = let
    wrds = words s
    wrd = head wrds
  in if wrd `isPrefixOf` r
    then matchStringM (unwords (tail wrds)) m{rs = drop (length wrd) r
                                             ,hi = h ++ greenString wrd}
    else matchStringM s m{rs = tail r
                         ,hi = h ++ [head r]}

matchStringMs :: String -> [Meta] -> [Meta]
#if 0
matchStringMs []     = id
matchStringMs (c:cs) = matchStringMs cs . matchCharMs c
#else
matchStringMs = matcherMap matchStringM
#endif
