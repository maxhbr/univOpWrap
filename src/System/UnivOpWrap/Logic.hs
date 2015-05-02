--------------------------------------------------------------------------------
-- |
-- Module      : UnivOpWrap.Logic
-- Note        :
--
--
--
--------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
module System.UnivOpWrap.Logic
  ( findMatchesI
  , findBestMatchI
  ) where

import           System.FilePath
import           System.HsTColors
import           System.UnivOpWrap.Common
import qualified System.UnivOpWrap.Logic.HeuristikA as A
import qualified System.UnivOpWrap.Logic.HeuristikB as B

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
    scanUpToBasename m@(Non _)  = m
    scanUpToBasename m@M{pr'=t} = let
        t' = splitFileName (snd t)
      in
        m{pr'=(fst t ++ fst t', snd t')}
  in
    f a (map scanUpToBasename l)

firstFilenameMs :: (a -> [MData] -> [MData]) -> a -> [MData] -> [MData]
firstFilenameMs f = concatMatcherMs (onlyFilenameMs f) f
