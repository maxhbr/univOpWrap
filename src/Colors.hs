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
colorPrint :: ANSIColor -> String -> IO ()
colorPrint c = putStrLn . colorString c

redPrint, greenPrint, yellowPrint, bluePrint, magentaPrint, cyanPrint, whitePrint :: String -> IO ()
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
