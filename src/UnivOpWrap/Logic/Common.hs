module UnivOpWrap.Logic.Common
  ( matcherMap
  ) where

import UnivOpWrap.Meta

matcherMap :: (a -> Meta -> Meta) -> a -> [Meta] -> [Meta]
matcherMap f a = map (f a)
