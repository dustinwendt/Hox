{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Event.Handler
import           Game
import           Text.Read             hiding (get)

data Format = Standard | Modern | Legacy | Vintage deriving Show

instance Read Format where
  readsPrec _ s =
    case s of
      "standard" -> [(Standard, "")]
      "s"        -> [(Standard, "")]
      "modern"   -> [(Modern, "")]
      "m"        -> [(Modern, "")]
      "legacy"   -> [(Legacy, "")]
      "l"        -> [(Legacy, "")]
      "vintage"  -> [(Vintage, "")]
      "v"        -> [(Vintage, "")]
      _          -> []

formatPrompt :: IO Format
formatPrompt = do
  putStr "Format (s/m/l/v): "
  x <- readMaybe <$> getLine
  maybe formatPrompt return x

playersPrompt :: IO Int
playersPrompt = do
  putStr "Number of Players: "
  x <- readMaybe <$> getLine
  case x of
    Just x -> if x >= 2 && x <= 8
              then return x
              else do
                  putStrLn "Please enter a number between 2 and 8"
                  playersPrompt
    Nothing -> playersPrompt

type EventSource a = (AddHandler a, a -> IO ())

makeSources = newAddHandler

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd

-- eventBoxCallback :: EventM EMotion Bool
-- eventBoxCallback = do
--   modifiers <- eventModifierMouse
--   when (Button1 `elem` modifiers) $ do
--     (x,y) <- eventCoordinates
--     liftIO $ print (x,y)
--   return True

main = gameLoop
 -- -- let networkDescription :: MomentIO ()
 --  --     networkDescription = do
 --  --       (event, eventSink) <- newEvent
 --  --       -- let modifiers = fmap eventModifier event in
 --  --       reactimate $
 --  --         (\_ -> do
 --  --                    -- (x,y) <- eventCoordinates
 --  --            let (x,y) = (1,1)
 --  --            liftIO $ labelSetText pName (T.pack (show (x,y)))) <$> (fmap eventModifier event)
 --        -- _ <- evbox `on` buttonPressEvent $ liftIO $ eventSink =<< eventButton
 --       -- return ()

 --  -- let networkDescription :: MomentIO ()
 --  --     networkDescription = do
 --  --       (event, eventSink) <- newEvent
 --  --       modifiers <- fmap eventModifier event
 --  --       return ()
 --  --       when (Button1 `elem` modifiers) $ do


 --    -- motionEvent <- fromAddHandler $ eventM (toWidget evbox) MotionNotify

 --    -- reactimate $ (\text -> labelSetText pName ) <$> textBehavior
 --  -- mainWindow `onMotionNotify` handleMotionEvent
 --  -- widgetAddEvents evbox [ButtonPressMask, PointerMotionMask]
 --  -- on mainWindow (fromLabel @"destroy" :: String) GI.mainQuit

 --  on mainWindow #destroy GI.mainQuit

 --  -- network <- compile networkDescription
 --  -- actuate network

-- main'' = do
--   initGUI
--   home <- getHomeDirectory

--   window1 <- windowNew

--   -- Frames
--   frame1 <- frameNew
--   frame2 <- frameNew

--   -- Windows
--   window1 <- windowNew
--   windowSetPosition window1 WinPosCenter
--   set window1 [ containerBorderWidth := 10
--               , windowTitle := "Hox"
--               , windowResizable := True
--               , windowDefaultWidth := 600
--               , windowDefaultHeight := 300]

--   sandBoxWindow <- windowNew
--   windowSetPosition sandBoxWindow WinPosCenter
--   set sandBoxWindow [ containerBorderWidth := 10
--               , windowTitle := "Sandbox"
--               , windowResizable := True
--               , windowDefaultWidth := 600
--               , windowDefaultHeight := 300]

--   on window1 objectDestroy mainQuit
--   on sandBoxWindow objectDestroy mainQuit

--   -- Widgets
--   entry <- entryNew
--   fileChooser <- fileChooserButtonNew "Deck" FileChooserActionOpen
--   button <- buttonNew
--   set button [ buttonLabel := "Validate" ]
--   on button buttonActivated $ do
--     fn <- fileChooserGetFilename fileChooser
--     case fn of
--       Just x  -> do
--         putStrLn $ "Validating " ++ x
--         handle <- openFile x ReadMode
--         contents <- hGetContents handle
--         let d = deckList contents

--         print $ "Deck size: " ++ show (sum (M.elems d))
--         hClose handle
--         if validDeck d
--         then do
--           widgetHide window1
--           builder <- builderNew
--           currDir <- getCurrentDirectory
--           builderAddFromFile builder (currDir ++ "/src/gameUI.glade")
--           sandBoxWindow <- builderGetObject builder castToWindow "sandbox_window"

--           widgetShowAll sandBoxWindow
--         else
--           putStrLn "Invalid Deck"
--       Nothing -> putStrLn "No file selected"

--   on fileChooser fileChooserButtonFileSet $ do
--     Just fn <- fileChooserGetFilename fileChooser
--     putStrLn $ "Selected: " ++ fn

--   -- Grids
--   grid1 <- gridNew
--   gridSetColumnSpacing grid1 10
--   gridSetRowSpacing grid1 10
--   gridSetColumnHomogeneous grid1 True

--   gridAttach grid1 fileChooser 0 0 3 2
--   gridAttach grid1 button 6 0 1 1

--   -- Containers
--   containerAdd window1 frame1
--   containerAdd frame1 grid1

--   -- set window1 [ windowTitle := "Hox", containerChild := button ]
--   widgetShowAll window1

--   mainGUI
