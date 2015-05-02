{-# LANGUAGE CPP #-}
module System.UnivOpWrap.Logic.HeuristikB
  ( matchStringM, matchStringMs
  ) where

import Data.List (isPrefixOf)
import System.HsTColors
import System.UnivOpWrap.Common

matchStringM :: String -> MData -> MData
matchStringM s = let
    matchStringM' :: [String] -> MData -> MData
    matchStringM' _ m@(Non _)                                   = m
    matchStringM' [] m                                          = m
    matchStringM' (w:ws) m@M{pr' = (hi,rs)} | null rs           = Non m
                                            | w `isPrefixOf` rs = matchStringM'
                                                ws
                                                m{pr' = (hi ++ greenString w
                                                        ,drop (length w) rs)}
                                            | otherwise         = matchStringM'
                                                (w:ws)
                                                m{pr' = (hi ++ [head rs]
                                                        ,tail rs)}
  in
    matchStringM' (words s)

matchStringMs :: String -> [MData] -> [MData]
matchStringMs s = map $ matchStringM s
