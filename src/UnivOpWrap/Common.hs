module UnivOpWrap.Common
  ( saveFile
  , Parameter (..), defaultParameter
  , Info (..)
  , Command (..)
  , MData (..), fn, pr, pr1, pr2, met, lst, isNotNon
  , newMData, mToLine, mFromLine
  , cleanPath
  )where

import Prelude hiding (readFile, writeFile)
import System.FilePath
import Data.Time
import System.Directory (getHomeDirectory)
import Data.List
import Data.Hashable
import System.Path.NameManip (guess_dotdot, absolute_path)
import Data.Maybe

import UnivOpWrap.Config
import System.HsTColors


saveFile :: Command -> IO FilePath
saveFile (C c) = do
    s <- cleanPath saveDir
    return $ s </> show (hash c)

--------------------------------------------------------------------------------
--  Data definitions

data Parameter = P { cmP   :: Maybe Command
                   , argsP :: [String]
                   , list  :: Bool
                   , fork  :: Bool
                   , ask   :: Bool
                   , tui   :: Bool
                   , dbg   :: Bool }
defaultParameter :: Parameter
defaultParameter = P Nothing [] False False False False False

data Info = I { cm :: Command  -- the command
              , sf :: FilePath -- the path, where the MData is saved
              , md :: [MData]  -- the corresponding MData
              } deriving (Show)
 
data Command = C FilePath
  deriving (Eq)
instance Show Command where
    show (C c) = c

data MData = M { fn'  :: FilePath        -- path of file
               , pr'  :: (String,String) -- start + end
               , met' :: Int             -- value of metric
               , lst' :: Day             -- last opened
               } | Non MData
fn :: MData -> FilePath
fn m@M{fn'=f}       = f
fn (Non m@M{fn'=f}) = f
fn (Non (Non _))    = error "MData should not be nested"
pr :: MData -> (String, String)
pr m@M{pr'=t}       = t
pr (Non m@M{pr'=t}) = t
pr (Non (Non _))    = error "MData should not be nested"
pr1, pr2 :: MData -> String
pr1 = fst . pr
pr2 = snd . pr
met :: MData -> Int
met m@M{met'=v}       = v
met (Non m@M{met'=v}) = v
met (Non (Non _))     = error "MData should not be nested"
lst :: MData -> Day
lst m@M{lst'=d}       = d
lst (Non m@M{lst'=d}) = d
lst (Non (Non _))     = error "MData should not be nested"

isNotNon :: MData -> Bool
isNotNon m@M{}   = True
isNotNon (Non _) = False

instance Eq MData where
  M{fn'=f} == M{fn'=f'} = f' == f
  Non _    == M{}       = False
  M{}      == Non _     = False
  Non _    == Non _     = True

instance Ord MData where
  M{met'=m} `compare` M{met'=m'} = m' `compare` m
  Non _     `compare` M{}        = LT
  M{}       `compare` Non _      = GT
  Non _     `compare` Non _      = EQ

instance Show MData where
  show (Non m)            = "[Non " ++ show m ++ "]"
  show m@M{met'=v,lst'=d} = pr1 m ++ pr2 m
                         ++ " (" ++ show v ++ ", " ++ show d ++ ")"

--------------------------------------------------------------------------------
--  Functions

newMData :: FilePath -> Int -> IO MData
newMData f i = do
  f' <- cleanPath f
  t <- getCurrentTime
  return $ M f ("",f') i (utctDay t)

mToLine :: MData -> String
mToLine m = unwords [ show (lst m)
                    , show (met m)
                    , fn m
                    ]

mFromLine :: String -> Maybe MData
mFromLine l = let
    ws = words l
  in if length ws >= 3
    then let
        f = unwords (drop 2 ws)
      in Just M { fn'  = f
                , pr'  = ("",f)
                , met' =  read (ws!!1)
                , lst' = utctDay $ read $ head ws ++ " 00:00:00.00000 UTC"
                }
    else Nothing

-- |makes paths absolute
cleanPath :: String -> IO String
cleanPath p | "~" `isPrefixOf` p = do
    homePath <- getHomeDirectory
    return $ normalise $ addTrailingPathSeparator homePath ++ tail p
             | otherwise          = do
    pathMaybeWithDots <- absolute_path p
    return $ fromJust $ guess_dotdot pathMaybeWithDots
