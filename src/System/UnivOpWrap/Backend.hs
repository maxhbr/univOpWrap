{-# LANGUAGE DeriveGeneric #-}
module System.UnivOpWrap.Backend
  ( getInfo
  , updateInfo
  , saveInfo
  , updAndSvInfo
  ) where

import Prelude hiding (readFile, writeFile)
import System.Directory
import System.FilePath
import Data.List
import Data.Time
import Data.Maybe
import Data.Text (pack, unpack)
import Data.ByteString.Lazy hiding (map)
import GHC.Generics
import Data.Aeson
import System.UnivOpWrap.Common

data InfoPre = IPre Command [MDataPre] deriving Generic
data MDataPre = MPre { mPrefn  :: String
                     , mPremet :: Int
                     , mPrelst :: String } deriving Generic

instance FromJSON InfoPre
instance ToJSON InfoPre

instance FromJSON MDataPre
instance ToJSON MDataPre

instance FromJSON Command
instance ToJSON Command

mdataFromPre :: MDataPre -> MData
mdataFromPre (MPre f m l) = M f
                              ("",f)
                              m
                              (utctDay $ read $ l ++ " 00:00:00.00000 UTC")
mdataToPre :: MData -> MDataPre
mdataToPre (Non m)                   = mdataToPre m
mdataToPre m@M{fn'=f,met'=m',lst'=l} = MPre f m' (show l)

infoFromPre :: Maybe InfoPre -> Info
infoFromPre Nothing            = undefined
infoFromPre (Just (IPre c ms)) = I c $ map mdataFromPre ms
infoToPre :: Info -> InfoPre
infoToPre i@I{cm=c,md=ms} = IPre c $ map mdataToPre ms

--------------------------------------------------------------------------------
--
getInfo :: Command -> IO Info
getInfo c = do
    s <- saveFile c
    ex <- doesFileExist s
    if ex
      then do
        cont <- readFile s
        return $ infoFromPre (decode cont :: Maybe InfoPre)
      else
        return I { cm = c, md = [] }

updateInfo :: MData -> Info -> Info
updateInfo m i@I{md=ms} = let
    mDown, mUp :: Int -> Int
    mDown = (`div` 2)
    mUp   = (+ 100)

    updateMData' :: MData -> [MData] -> [MData]
    updateMData' (Non _) []            = []
    updateMData' n@(Non _) (m':ms)     = m'{met' = mDown (met m')}
                                       : updateMData' n ms
    updateMData' m []                  = [m]
    updateMData' m (m':ms) | m == m'   = m'{met' = mUp (met m')}
                                       : updateMData' (Non m) ms
                           | otherwise = m'{met' = mDown (met m')}
                                       : updateMData' m ms
  in
    i { md = sort (updateMData' m ms)}

saveInfo :: Info -> IO ()
saveInfo i = do
    s <- saveFile (cm i)
    createDirectoryIfMissing False (takeDirectory s)
    writeFile s $ encode (infoToPre i)

updAndSvInfo :: MData -> Info -> IO()
updAndSvInfo m = saveInfo . updateInfo m

-- |Delets unused / notexistend files
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
