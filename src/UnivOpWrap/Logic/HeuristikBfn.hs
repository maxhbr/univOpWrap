{-# LANGUAGE CPP #-}
module UnivOpWrap.Logic.HeuristikBfn
  ( matchStringM, matchStringMs
  ) where

import Data.List (isPrefixOf)
import System.HsTColors
import UnivOpWrap.Common
import UnivOpWrap.Logic.Common
import System.FilePath

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
    scanUpToBasename :: MData -> MData
    scanUpToBasename m@(Non _) = m
    scanUpToBasename m         = m{pr'=(takeDirectory (pr2 m) ++ "/"
                                       ,takeFileName (pr2 m))}
  in
    matchStringM' (words s) . scanUpToBasename

matchStringMs :: String -> [MData] -> [MData]
matchStringMs = matcherMap matchStringM
