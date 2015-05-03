--------------------------------------------------------------------------------
-- |
-- Module      : System.HsTColors
-- Note        :
--
-- see: https://github.com/maximilianhuber/hsTColors
-- Based on: https://github.com/schell/steeloverseer/blob/master/src/ANSIColors.hs
--
--------------------------------------------------------------------------------
module System.HsTColors
  where

import Debug.Trace (trace)

data ANSIColor = ANSIBlack
               | ANSIRed
               | ANSIGreen
               | ANSIYellow
               | ANSIBlue
               | ANSIMagenta
               | ANSICyan
               | ANSIWhite
               | ANSINone
    deriving (Ord, Eq)

instance Show ANSIColor where
    show ANSINone = "\27[0m"
    show c = let
        colorNum = length $ takeWhile (/= c) [ ANSIBlack
                                             , ANSIRed
                                             , ANSIGreen
                                             , ANSIYellow
                                             , ANSIBlue
                                             , ANSIMagenta
                                             , ANSICyan
                                             , ANSIWhite ]
      in "\27[" ++ show (30 + colorNum) ++ "m"

uncolor :: String -> String
uncolor "" = ""
uncolor (s:ss) | s == '\27' = let 
    uncolorDrop ""       = ""
    uncolorDrop ('m':ss') = ss'
    uncolorDrop (_:ss')   = uncolorDrop ss'
  in
    uncolor (uncolorDrop ss)
               | otherwise  = s : uncolor ss

--------------------------------------------------------------------------------
--  String coloring
colorString :: ANSIColor -> String -> String
colorString c s = show c ++ s ++ show ANSINone

redString, greenString, yellowString, blueString, magentaString, cyanString, whiteString :: String -> String
redString = colorString ANSIRed
greenString = colorString ANSIGreen
yellowString = colorString ANSIYellow
blueString = colorString ANSIBlue
magentaString = colorString ANSIMagenta
cyanString = colorString ANSICyan
whiteString = colorString ANSIWhite

--------------------------------------------------------------------------------
--  Put colored strings
colorPutStrLn :: ANSIColor -> String -> IO ()
colorPutStrLn c = putStrLn . colorString c

redPutStrLn, greenPutStrLn, yellowPutStrLn, bluePutStrLn, magentaPutStrLn, cyanPutStrLn, whitePutStrLn :: String -> IO ()
redPutStrLn = colorPutStrLn ANSIRed
greenPutStrLn = colorPutStrLn ANSIGreen
yellowPutStrLn = colorPutStrLn ANSIYellow
bluePutStrLn = colorPutStrLn ANSIBlue
magentaPutStrLn = colorPutStrLn ANSIMagenta
cyanPutStrLn = colorPutStrLn ANSICyan
whitePutStrLn = colorPutStrLn ANSIWhite

colorPutStr :: ANSIColor -> String -> IO ()
colorPutStr c = putStr . colorString c

redPutStr, greenPutStr, yellowPutStr, bluePutStr, magentaPutStr, cyanPutStr, whitePutStr :: String -> IO ()
redPutStr = colorPutStr ANSIRed
greenPutStr = colorPutStr ANSIGreen
yellowPutStr = colorPutStr ANSIYellow
bluePutStr = colorPutStr ANSIBlue
magentaPutStr = colorPutStr ANSIMagenta
cyanPutStr = colorPutStr ANSICyan
whitePutStr = colorPutStr ANSIWhite

--------------------------------------------------------------------------------
--  Print colored things
colorPrint :: Show a => ANSIColor -> a -> IO ()
colorPrint c = putStrLn . colorString c . show

redPrint, greenPrint, yellowPrint, bluePrint, magentaPrint, cyanPrint, whitePrint :: Show a => a -> IO ()
redPrint = colorPrint ANSIRed
greenPrint = colorPrint ANSIGreen
yellowPrint = colorPrint ANSIYellow
bluePrint = colorPrint ANSIBlue
magentaPrint = colorPrint ANSIMagenta
cyanPrint = colorPrint ANSICyan
whitePrint = colorPrint ANSIWhite

--------------------------------------------------------------------------------
--  Trace colored strings
colorTrace :: ANSIColor -> String -> a -> a
colorTrace c s = trace (colorString c s)

redTrace, greenTrace, yellowTrace, blueTrace, magentaTrace, cyanTrace, whiteTrace :: String -> a -> a
redTrace = colorTrace ANSIRed
greenTrace = colorTrace ANSIGreen
yellowTrace = colorTrace ANSIYellow
blueTrace = colorTrace ANSIBlue
magentaTrace = colorTrace ANSIMagenta
cyanTrace = colorTrace ANSICyan
whiteTrace = colorTrace ANSIWhite

--------------------------------------------------------------------------------
--  IdTrace colored strings
colorIdTrace :: ANSIColor -> String -> String
colorIdTrace a s = trace (colorString a s) s

redIdTrace, greenIdTrace, yellowIdTrace, blueIdTrace, magentaIdTrace, cyanIdTrace, whiteIdTrace :: String -> String
redIdTrace = colorIdTrace ANSIRed
greenIdTrace = colorIdTrace ANSIGreen
yellowIdTrace = colorIdTrace ANSIYellow
blueIdTrace = colorIdTrace ANSIBlue
magentaIdTrace = colorIdTrace ANSIMagenta
cyanIdTrace = colorIdTrace ANSICyan
whiteIdTrace = colorIdTrace ANSIWhite

--------------------------------------------------------------------------------
--  Show colored things
colorShow :: Show a => ANSIColor -> a -> String
colorShow c s = colorString c (show s)

redShow, greenShow, yellowShow, blueShow, magentaShow, cyanShow, whiteShow :: Show a => a -> String
redShow = colorShow ANSIRed
greenShow = colorShow ANSIGreen
yellowShow = colorShow ANSIYellow
blueShow = colorShow ANSIBlue
magentaShow = colorShow ANSIMagenta
cyanShow = colorShow ANSICyan
whiteShow = colorShow ANSIWhite
