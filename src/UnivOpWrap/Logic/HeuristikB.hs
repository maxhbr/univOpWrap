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
matchStringM s = let
    matchStringM' :: [String] -> Meta -> Meta
    matchStringM' _ Non                  = Non
    matchStringM' [] m                   = m
    matchStringM' _ M{rs = []}           = Non
    matchStringM' (w:ws) m@M{rs=r, hi=h} = if w `isPrefixOf` r
      then matchStringM' ws m{rs = drop (length w) r
                             ,hi = h ++ greenString w}
      else matchStringM' (w:ws) m{rs = tail r
                                 ,hi = h ++ [head r]}
  in
    matchStringM' (words s)

matchStringMs :: String -> [Meta] -> [Meta]
#if 0
matchStringMs []     = id
matchStringMs (c:cs) = matchStringMs cs . matchCharMs c
#else
matchStringMs = matcherMap matchStringM
#endif
