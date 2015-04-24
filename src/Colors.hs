--------------------------------------------------------------------------------
-- |
-- Module      : Colors
-- Note        :
--
-- Stolen from: https://github.com/schell/steeloverseer/blob/master/src/ANSIColors.hs
--
--------------------------------------------------------------------------------
module Colors
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
--  Print colored strings
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
--  Trace colored strings
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
