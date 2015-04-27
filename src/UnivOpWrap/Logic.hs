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
  ) where

import System.HsTColors
import UnivOpWrap.Common
-- import qualified UnivOpWrap.Logic.HeuristikA as A
-- import qualified UnivOpWrap.Logic.HeuristikB as B

concatMatcherMs :: (a -> Info -> Info)
                -> (a -> Info -> Info)
                -> a -> Info -> Info
concatMatcherMs = undefined

findMatches :: String -> Info -> Info
findMatches _ = id

findBestMatch:: String -> Info -> MData
findBestMatch _ i@I{md=m} = head m

-- matchStringMs = concatMatcherMs B.matchStringMs A.matchStringMs

-- findMatches :: ([Meta], String) -> [Meta]
-- #if 0
-- -- This variant might be usefull in the interactive case. It has transposed
-- -- lazyness
-- findMatches (ms,n) = [m | m <- A.matchStringMs n ms, m /= Non]
-- #else
-- findMatches (ms,n) = [m | m <- matchStringMs n ms, m /= Non]
-- #endif

-- findBestMatch :: ([Meta], String) -> Meta
-- findBestMatch (ms,n) = let
--     mtches = [m | m <- matchStringMs n ms, m /= Non]
--   in if not (null mtches)
--     then head mtches
--     else Non
