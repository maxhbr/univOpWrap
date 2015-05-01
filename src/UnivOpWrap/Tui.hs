{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module UnivOpWrap.Tui 
  where
  -- ( runTui
  -- ) where

-- import Graphics.Vty hiding (Button)
-- import Graphics.Vty.Widgets.All
-- import Data.Text hiding (head)

-- import UnivOpWrap.Meta

-- runTui :: String -> [Meta] -> IO()
-- runTui c ms = let 
--     genTui = do
--       e <- editWidget
-- #if 1
--       -- TODO: textList is false?
--       lst <- newTextList [] 1
--       mapM_ (addMeta lst) ms
-- #else
--       lst' <- multiLineEditWidget
--       lst <- bordered =<< boxFixed 60 20 lst'
-- #endif
--       fg <- newFocusGroup
--       _ <- addToFocusGroup fg e
--       _ <- addToFocusGroup fg lst
--       be <- bordered =<< boxFixed 60 1 e
--       c <- centered =<< ( plainText (pack $ c ++ ":")
--                      <--> return be
--                      <--> plainText "Files:"
--                      <--> return lst)
--       coll <- newCollection
--       _ <- addToCollection coll c fg
--       return (coll, fg, lst)
--     addMeta _ Non         = return ()
--     addMeta lst m@M{fn=f} = plainText (pack f) >>= addToList lst "unused"
--   in do
--     (coll,fg,lst) <- genTui
--     fg `onKeyPressed` \_ k _ ->
--       case k of
--         KEsc -> shutdownUi >> return True
--         _ -> return False
--     runUi coll $ defaultContext { focusAttr = fgColor yellow }

-- -- module Main where


-- -- main :: IO ()
-- -- main = do
-- --   e1 <- multiLineEditWidget
-- --   e2 <- multiLineEditWidget
-- --   setEditLineLimit e2 $ Just 3
-- --   e3 <- editWidget
-- --   e4 <- editWidget
-- --   setEditRewriter e4 (const '*')

-- --   fg <- newFocusGroup
-- --   _ <- addToFocusGroup fg e1
-- --   _ <- addToFocusGroup fg e2
-- --   _ <- addToFocusGroup fg e3
-- --   _ <- addToFocusGroup fg e4

-- --   be1 <- bordered =<< boxFixed 40 5 e1
-- --   be2 <- bordered =<< boxFixed 40 3 e2
-- --   be3 <- bordered =<< boxFixed 40 1 e3
-- --   be4 <- bordered =<< boxFixed 40 1 e4

-- --   c <- centered =<< ((plainText "Multi-Line Editor (unlimited lines):")
-- --                          <--> (return be1)
-- --                          <--> (plainText "Multi-Line Editor (3 lines):")
-- --                          <--> (return be2)
-- --                          <--> (plainText "Single-Line Editor:")
-- --                          <--> (return be3)
-- --                          <--> (plainText "Password input:")
-- --                          <--> (return be4)
-- --                          <--> (plainText "- Esc to quit\n\n- TAB to switch editors") >>= withBoxSpacing 1
-- --                     )

-- --   coll <- newCollection
-- --   _ <- addToCollection coll c fg

-- --   fg `onKeyPressed` \_ k _ ->
-- --       case k of
-- --         KEsc -> shutdownUi >> return True
-- --         _ -> return False

-- --   runUi coll $ defaultContext { focusAttr = fgColor yellow }
