module UnivOpWrap.Logic.Common
  ( matcherMap
  , concatMatcherMs
  , onlyFilenameMs
  , firstFilenameMs
  ) where

import UnivOpWrap.Common
import System.FilePath

matcherMap :: (a -> MData -> MData) -> a -> [MData] -> [MData]
matcherMap f k = map (f k)

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
