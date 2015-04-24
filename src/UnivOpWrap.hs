--------------------------------------------------------------------------------
-- | 
-- Module      : UnivOpWrap
-- Note        : 
-- 
-- 
-- 
--------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
module UnivOpWrap
  ( defaultRoutine
  , sanitizeMetaFromCommand
  ) where

import System.Directory
import UnivOpWrap.Meta
import UnivOpWrap.Logic

defaultRoutine :: String -> [String] -> IO()
defaultRoutine c = let 
    defaultRoutine' n = do
      meta <- getMetaFromCommand c
      ex <- doesFileExist n
      meta' <- if ex
        then do
          putStrLn $ "Call function with file " ++ n
          return (updateMeta n meta)
        else do 
          let mtch = findBestMatch (meta,n)
          putStrLn $ "Found file " ++ show mtch
          return (updateMeta (fn mtch) meta)
      showMetas meta'
  in mapM_ defaultRoutine'
