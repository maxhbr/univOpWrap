{-# LANGUAGE CPP #-}
module UnivOpWrap.Logic.HeuristikA
  ( matchStringM, matchStringMs
  ) where

import System.HsTColors
import UnivOpWrap.Common
import UnivOpWrap.Logic.Common

matchCharM :: Char -> MData -> MData
matchCharM _ m@(Non _)                         = m
matchCharM c m@M{pr' = (hi,rs)} | null rs      = Non m
                                | head rs == c =
                                    m{pr'=(hi ++ greenString [c],tail rs)}
                                | otherwise    = matchCharM c $
                                    m{pr'=(hi ++ [head rs],tail rs)}

matchStringM :: String -> MData -> MData
matchStringM []     = id
matchStringM (c:cs) = matchStringM cs . matchCharM c

matchStringMs :: String -> [MData] -> [MData]
matchStringMs = matcherMap matchStringM
