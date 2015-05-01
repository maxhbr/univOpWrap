module UnivOpWrap.Logic.Common
  ( matcherMap
  , concatMatcherMs
  ) where

import UnivOpWrap.Common

matcherMap :: (a -> MData -> MData) -> a -> [MData] -> [MData]
matcherMap f k = map (f k)

concatMatcherMs :: (a -> [MData] -> [MData])
                -> (a -> [MData] -> [MData])
                -> a -> [MData] -> [MData]
concatMatcherMs f g a l = f a l ++ g a l
