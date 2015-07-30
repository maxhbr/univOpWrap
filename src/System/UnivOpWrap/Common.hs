{-# LANGUAGE DeriveGeneric #-}
module System.UnivOpWrap.Common
  ( saveFile
  , Parameter (..), defaultParameter
  , Info (..)
  , Command (..), commandFromString
  , MData (..), fn, pr, pr1, pr2, met, lst
  , isNotNon, takeOnlyNotNon, countNotNon
  , newMData
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
import GHC.Generics

import System.UnivOpWrap.Config


saveFile :: Command -> IO FilePath
saveFile (C c) = do
    s <- cleanPath saveDir
    return $ s </> ("uow" ++ show (hash (show c)))

--------------------------------------------------------------------------------
--  Data definitions

data Parameter = P { cmP      :: Maybe Command
                   , argsP    :: [String]
                   , list     :: Bool
                   , repl     :: Bool
                   , fork     :: Bool
                   , ask      :: Bool
                   , tui      :: Bool
                   , sanitize :: Bool
                   , dbg      :: Bool }
defaultParameter :: Parameter
defaultParameter = P Nothing [] False False False False False False False

data Info = I { cm :: Command  -- the command
              , md :: [MData]  -- the corresponding MData
              } deriving (Show)

data Command = C [(String,FilePath)]
  deriving (Eq, Generic)
instance Show Command where
    show (C cps) = show cps

data MData = M { fn'  :: FilePath        -- path of file
               , pr'  :: (String,String) -- start + end
               , met' :: Int             -- value of metric
               , lst' :: Day             -- last opened
               } | Non MData
fn :: MData -> FilePath
fn M{fn'=f}       = f
fn (Non M{fn'=f}) = f
fn (Non (Non _))  = error "MData should not be nested"
pr :: MData -> (String, String)
pr M{pr'=t}       = t
pr (Non M{pr'=t}) = t
pr (Non (Non _))  = error "MData should not be nested"
pr1, pr2 :: MData -> String
pr1 = fst . pr
pr2 = snd . pr
met :: MData -> Int
met M{met'=v}       = v
met (Non M{met'=v}) = v
met (Non (Non _))   = error "MData should not be nested"
lst :: MData -> Day
lst M{lst'=d}       = d
lst (Non M{lst'=d}) = d
lst (Non (Non _))   = error "MData should not be nested"

isNotNon :: MData -> Bool
isNotNon (Non _) = False
isNotNon _       = True

countNotNon :: [MData] -> Int
countNotNon ms = let
    countNotNon' (Non _:ms) i = countNotNon' ms i
    countNotNon' (m:ms)     i = countNotNon' ms $ i + 1
    countNotNon' []         i = i
  in countNotNon' ms 0

takeOnlyNotNon :: [MData] -> [MData]
takeOnlyNotNon ms = [m | m <- ms, isNotNon m]

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

commandFromString :: String -> Command
commandFromString s = let
    getCmdPerFT :: [String] -> [(String,FilePath)]
    getCmdPerFT []             = []
    getCmdPerFT (('.':cs):css) = let
        spltCmd :: String -> [String]
        spltCmd s = case dropWhile (==':') s of
            "" -> []
            s' -> w : words s''
                  where (w, s'') = break (==':') s'
        splt = spltCmd cs
      in ('.' : head splt,  tail $ splt !! 1) : getCmdPerFT css
    getCmdPerFT (c:css)        = ("",c) : getCmdPerFT css
  in C . getCmdPerFT $ words s

newMData :: FilePath -> Int -> IO MData
newMData f i = do
  f' <- cleanPath f
  t <- getCurrentTime
  return $ M f ("",f') i (utctDay t)

-- |makes paths absolute
cleanPath :: String -> IO String
cleanPath p | "~" `isPrefixOf` p = do
  homePath <- getHomeDirectory
  return $ normalise $ addTrailingPathSeparator homePath ++ tail p
           | otherwise          = do
  pathMaybeWithDots <- absolute_path p
  return $ fromJust $ guess_dotdot pathMaybeWithDots
