--------------------------------------------------------------------------------
-- | 
-- Module      : UnivOpWrap.Meta
-- Note        : 
-- 
-- 
-- 
--------------------------------------------------------------------------------
module UnivOpWrap.Meta
  ( getMetaFromCommand
  , sanitizeMetaFromCommand
  , updateMeta
  ) where
--------------------------------------------------------------------------------
--  Data definitions

data Meta = M { metric :: Int, fileName :: String}

--------------------------------------------------------------------------------
--

sortByMetric :: [Meta] -> [Meta]
sortByMetric = id

-- |Obtains the 'meta-info' corresponding to some command
-- It returns a list of files, where the first file is the most important
getMetaFromCommand :: String -> IO [String]
getMetaFromCommand _ = return $ map fileName $ sortByMetric
  [ M {metric = 1, fileName ="/etc/fstab"}
  , M {metric = 1, fileName ="/etc/vimrc"}
  , M {metric = 1, fileName ="/etc/hosts"}
  , M {metric = 1, fileName ="/home/USER/.vimrc"}
  , M {metric = 1, fileName ="/home/USER/.zshrc"}]

updateMeta :: [Meta] -> String -> [Meta]
updateMeta = let
    changeMetrikUp   i = i + 100
    changeMetrikDown i = i `div` 2
  in undefined

-- |Delets unused / notexistend files
sanitizeMetaFromCommand :: String -> IO ()
sanitizeMetaFromCommand = undefined

