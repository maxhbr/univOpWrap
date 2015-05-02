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

import System.FilePath
import System.HsTColors
import UnivOpWrap.Common
import qualified UnivOpWrap.Logic.HeuristikA as A
import qualified UnivOpWrap.Logic.HeuristikB as B

findMatchesI :: String -> Info -> Info
findMatchesI s i@I{md=ms} = i{md = findMatches s ms}
findBestMatchI :: String -> Info -> MData
findBestMatchI s = findBestMatch s . md

findMatches :: String -> [MData] -> [MData]
findMatches = concatMatcherMs (firstFilenameMs B.matchStringMs) A.matchStringMs

findBestMatch:: String -> [MData] -> MData
findBestMatch s ms = head [m | m <- findMatches s ms, isNotNon m]

--------------------------------------------------------------------------------
--  Helper

concatMatcherMs :: (a -> [MData] -> [MData])
                -> (a -> [MData] -> [MData])
                -> a -> [MData] -> [MData]
concatMatcherMs f g a l = f a l ++ g a l

onlyFilenameMs :: (a -> [MData] -> [MData]) -> a -> [MData] -> [MData]
onlyFilenameMs f a l = let
    scanUpToBasename :: MData -> MData
    scanUpToBasename m@(Non _) = m
    scanUpToBasename m         = m{pr'=(takeDirectory (pr2 m) ++ "/"
                                       ,takeFileName (pr2 m))}
  in
    f a (map scanUpToBasename l)

firstFilenameMs :: (a -> [MData] -> [MData]) -> a -> [MData] -> [MData] 
firstFilenameMs f = concatMatcherMs (onlyFilenameMs f) f
