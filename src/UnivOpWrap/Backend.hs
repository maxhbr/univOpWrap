module UnivOpWrap.Backend
  ( getInfo
  , updateInfo
  , saveInfo
  , updAndSvInfo
  ) where

import Prelude hiding (readFile, writeFile)
import System.Directory
import System.FilePath
import Data.List
import Data.Maybe
import Data.Text (pack, unpack)
import Data.Text.IO
import UnivOpWrap.Common

-- |Obtains the 'meta-info' corresponding to some command
getInfo :: Command -> IO Info
getInfo c = do
    s <- saveFile c
    ex <- doesFileExist s
    if ex
      then do
        cont <- readFile s
        return I { cm = c
                 , sf = s
                 , md = mapMaybe mFromLine (lines (unpack cont)) }
      else
        return I { cm = c
                 , sf = s
                 , md = [] }

updateInfo :: MData -> Info -> Info
updateInfo m i@I{md=ms} = let
    mDown, mUp :: Int -> Int
    mDown = (`div` 2)
    mUp   = (+ 100)

    updateMData' :: MData -> [MData] -> [MData]
    updateMData' (Non _) []                       = []
    updateMData' n@(Non _) (m':ms)                = m'{met' = mDown (met m')}
                                                  : updateMData' n ms
    updateMData' m []                             = [m]
    updateMData' m@M{fn'=f} (m':ms) | f == fn m'  = m'{met' = mUp (met m')}
                                                  : updateMData' (Non m) ms
                                    | otherwise   = m'{met' = mDown (met m')}
                                                  : updateMData' m ms
  in
    i { md = sort (updateMData' m ms)}

saveInfo :: Info -> IO ()
saveInfo i@I{sf=s,md=m} = do 
    createDirectoryIfMissing False (takeDirectory s)
    writeFile s $ pack $ unlines $ map mToLine m

updAndSvInfo :: MData -> Info -> IO()
updAndSvInfo m = saveInfo . updateInfo m

-- -- |Delets unused / notexistend files
sanitizeInfo :: Info -> IO ()
sanitizeInfo = undefined
--     f Non       = return Non
--     f m@M{fn=f} = do
--       ex <- doesFileExist f
--       if ex
--         then return m
--         else return Non
--   in
--     loadMeta c >>= mapM f >>= saveMeta c
