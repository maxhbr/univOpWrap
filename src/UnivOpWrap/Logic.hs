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
import qualified UnivOpWrap.Logic.HeuristikA as A
import qualified UnivOpWrap.Logic.HeuristikB as B

concatMatcherMs :: (a -> [Meta] -> [Meta])
              -> (a -> [Meta] -> [Meta])
              -> a -> [Meta] -> [Meta]
concatMatcherMs f f' s ms = f s ms ++ f' s ms

matchStringMs = concatMatcherMs B.matchStringMs A.matchStringMs

findMatches :: ([Meta], String) -> [Meta]
#if 0
-- This variant might be usefull in the interactive case. It has transposed
-- lazyness
findMatches (ms,n) = [m | m <- A.matchStringMs n ms, m /= Non]
#else
findMatches (ms,n) = [m | m <- matchStringMs n ms, m /= Non]
#endif

findBestMatch :: ([Meta], String) -> Meta
findBestMatch (ms,n) = let
    mtches = [m | m <- matchStringMs n ms, m /= Non]
  in if not (null mtches)
    then head mtches
    else Non
