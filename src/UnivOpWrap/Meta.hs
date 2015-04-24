--------------------------------------------------------------------------------
-- | 
-- Module      : UnivOpWrap.Meta
-- Note        : 
-- 
-- 
-- 
--------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
module UnivOpWrap.Meta
  ( Meta (..), constM, showMetas
  , loadMeta
  , updateMeta, saveMeta
  , sanitizeMetaFromCommand
  ) where

import Prelude hiding (readFile, writeFile)
import System.Directory
import System.FilePath
import Data.List
import Data.Text (pack, unpack)
import Data.Text.IO

import Debug.Trace

saveDir :: String -> FilePath
saveDir c = "/tmp/UnivOpWrap/" </> c

--------------------------------------------------------------------------------
--  Data definitions
data Meta = M { met :: Int, fn :: FilePath , rs :: String, hi :: String } | Non
  deriving (Eq)

constM :: FilePath -> Meta
constM = constM' 0

constM' :: Int -> FilePath -> Meta
constM' i f = M i f f ""

instance Ord Meta where
  M{met=m} `compare` M{met=m'} = m' `compare` m
  Non `compare` M{}            = LT
  M{} `compare` Non            = GT
  Non `compare` Non            = EQ

instance Show Meta where
  show Non = "[Non]"
  show m   = hi m ++ rs m ++ " (" ++ show (met m) ++ ")"

showMetas :: [Meta] -> IO()
showMetas = mapM_ print

--------------------------------------------------------------------------------
--

-- |Obtains the 'meta-info' corresponding to some command
-- It returns a list of files, where the first file is the most important
loadMeta :: String -> IO [Meta]
loadMeta c = let
    toMeta :: String -> Meta
    toMeta s = let
        ws = words s
      in
        constM' (read (head ws) :: Int) (unwords (tail ws))
  in do
    ex <- doesFileExist (saveDir c)
    if ex
      then do
        cont <- readFile (saveDir c)
        return (map toMeta (lines (unpack cont)))
      else
        return []

updateMeta :: Meta -> [Meta] -> [Meta]
updateMeta = let
    mDown, mUp :: Int -> Int
    mDown = (`div` 2)
    mUp   = (+ 100)

    updateMeta' :: Meta -> [Meta] -> [Meta]
    updateMeta' Non []                         = []
    updateMeta' Non (m':ms)                    = m'{met = mDown (met m')}
                                               : updateMeta' Non ms
    updateMeta' M{fn=f} []                     = [M 100 f f ""]
    updateMeta' m@M{fn=f} (m':ms) | f == fn m' = m'{met = mUp (met m')}
                                               : updateMeta' Non ms
                                  | otherwise  = m'{met = mDown (met m')}
                                               : updateMeta' m ms
  in
    \ m -> sort . updateMeta' m

-- |Writes meta-data to the file corresponding to the given command
saveMeta :: String -> [Meta] -> IO ()
saveMeta c ms = let
    format m = show (met m) ++ " " ++ fn m
  in do
    createDirectoryIfMissing False (saveDir "")
    writeFile (saveDir c) (pack (unlines (map format ms)))

-- |Delets unused / notexistend files
sanitizeMetaFromCommand :: String -> IO ()
sanitizeMetaFromCommand = undefined
