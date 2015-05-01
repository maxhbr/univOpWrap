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
  ( findMatches,   findMatchesI
  , findBestMatch, findBestMatchI
  ) where

import System.HsTColors
import UnivOpWrap.Common
import UnivOpWrap.Logic.Common
import qualified UnivOpWrap.Logic.HeuristikA as A
import qualified UnivOpWrap.Logic.HeuristikB as B

findMatchesI :: String -> Info -> Info
findMatchesI s i@I{md=ms} = i{md = findMatches s ms}
findBestMatchI :: String -> Info -> MData
findBestMatchI s = findBestMatch s . md

findMatches :: String -> [MData] -> [MData]
findMatches = concatMatcherMs B.matchStringMs A.matchStringMs

findBestMatch:: String -> [MData] -> MData
findBestMatch s ms = head [m | m <- findMatches s ms, isNotNon m]
