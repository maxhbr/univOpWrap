--------------------------------------------------------------------------------
-- | 
-- Module      : UnivOpWrap.Helper
-- Note        : 
-- 
--
--------------------------------------------------------------------------------

module UnivOpWrap.Helper
  (cleanPath
  ) where

import System.Path.NameManip (guess_dotdot, absolute_path)
import System.FilePath (addTrailingPathSeparator, normalise)
import System.Directory (getHomeDirectory)
import Data.Maybe (fromJust)
import Data.List (isPrefixOf)

--------------------------------------------------------------------------------
--

-- |makes paths absolute
-- partly stolen from:
-- https://www.fpcomplete.com/user/dshevchenko/cookbook/transform-relative-path-to-an-absolute-path
cleanPath :: String -> IO String
cleanPath p | "~" `isPrefixOf` p = do
    homePath <- getHomeDirectory
    return $ normalise $ addTrailingPathSeparator homePath ++ tail p
             | otherwise          = do
    pathMaybeWithDots <- absolute_path p
    return $ fromJust $ guess_dotdot pathMaybeWithDots
