--------------------------------------------------------------------------------
-- | 
-- Module      : UnivOpWrap.Meta
-- Note        : 
-- 
-- 
-- 
--------------------------------------------------------------------------------
module UnivOpWrap.Meta
  ( Meta (..), showMetas
  , getMetaFromCommand
  , updateMeta, updateMetaM
  , sanitizeMetaFromCommand
  ) where

import System.Directory
import Data.List

import Colors

--------------------------------------------------------------------------------
--  Data definitions
data Meta = M { met :: Int, fn :: FilePath , rs :: String, hi :: String } | Non
  deriving (Eq)

instance Ord Meta where
  M{met=m} `compare` M{met=m'} = m' `compare` m
  Non `compare` M{}            = LT
  M{} `compare` Non            = GT
  Non `compare` Non            = EQ

instance Show Meta where
    show Non = "Non"
    show m   = hi m ++ rs m ++ " (" ++ show (met m) ++ ")"

showMetas :: [Meta] -> IO()
showMetas = mapM_ print

--------------------------------------------------------------------------------
--

-- |Obtains the 'meta-info' corresponding to some command
-- It returns a list of files, where the first file is the most important
getMetaFromCommand :: String -> IO [Meta]
getMetaFromCommand _ = return $ sort $ map (\ (i,s) -> M i s s "")
  [ (10,   "/etc/fstab")
  , (1,    "/etc/vimrc")
  , (1,    "/etc/hosts")
  , (1000, "/home/USER/.vimrc")
  , (100,  "/home/USER/.zshrc") ]

updateMeta :: String -> [Meta] -> [Meta]
updateMeta [] [] = []
updateMeta s []  = [M 100 s s ""]
updateMeta s (m:ms) | s == fn m = m{met = met m + 100} : updateMeta "" ms
                    | otherwise = m{met = met m `div` 2} : updateMeta s ms

updateMetaM :: FilePath -> [Meta] -> IO [Meta]
updateMetaM f m = do
    ex <- doesFileExist f
    if ex
      then return (updateMeta f m)
      else redPrint ("File \"" ++ f ++ "\" does not exist") >> return m

-- |Delets unused / notexistend files
sanitizeMetaFromCommand :: String -> IO ()
sanitizeMetaFromCommand = undefined

