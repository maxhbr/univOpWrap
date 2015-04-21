--------------------------------------------------------------------------------
-- | 
-- Module      : UnivOpWrap
-- Note        : 
-- 
-- 
-- 
--------------------------------------------------------------------------------

module UnivOpWrap
  ( defaultRoutine
  , sanitizeMetaFromCommand
  ) where

import Colors
import UnivOpWrap.Meta
import UnivOpWrap.Logic

defaultRoutine :: String -> [String] -> IO()
defaultRoutine c ns = do
  yellowPrint "start"
  meta <- getMetaFromCommand c
  cyanTrace c (findBestMatch (meta,ns))
  yellowPrint "stop"
