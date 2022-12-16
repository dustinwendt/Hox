{-# LANGUAGE OverloadedLabels #-}

module Main where

import           CardList
import           ComplexTypes
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import qualified Data.Map                        as M
import           GI.Gdk.Flags
import           GI.Gdk.Objects.MotionEvent
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.Abstract.Widget
import           Graphics.UI.Gtk.Gdk.Events
import           Graphics.UI.Gtk.Layout.Fixed
import           Graphics.UI.Gtk.Misc.EventBox
import           Linear.Affine
import           SDL.Input.Mouse
import           System.Console.ANSI
import           System.Directory
import           System.IO
import           Text.Read                       hiding (get)
import           Util

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

isSpace :: Char -> Bool
isSpace ' ' = True
isSpace _   = False

deckList :: String -> M.Map GameObject Int
deckList s = foldr (uncurry (M.insertWith (+)) . pLine) M.empty (lines s)
  where
    pLine s = let (a,b) = break isSpace s in
            (strToCard (stripCard b), read a :: Int)

validDeck :: M.Map GameObject Int -> Bool
validDeck m = sum (M.elems m) >= 60 && f (M.toList m)
  where f [] = True
        f ((k,v):xs) | not (isBasic k) && v > 4 = False
                     | otherwise = f xs


-- pointEnv = (uncurry Point) <$> UI.mousedown canvas

-- handleMotionEvent :: Event -> IO Bool
handleMotionEvent Motion {eventX = xMain, eventY = yMain, eventXRoot = xEvent, eventYRoot = yEvent} = do
  -- (x, y) <- windowGetPosition mainWindow
  print $ "Event at " ++ show xEvent ++ " " ++ show yEvent
  return True
handleMotionEvent _ = do
  print "Here"
  return True

-- getMousePosition = do
--   events <- pollEvents
--   -- mousePos <- getAbsoluteMouseLocation
--   print events

-- type WidgetMotionNotifyEventCallback = EventMotion -> IO Bool

-- data EventM a = EventMError | Running a

-- instance Show a => Show (EventM a) where
--   show EventMError = "EventMError"
--   show (Running a) = "Running " ++ show a

-- instance Functor EventM where
--   fmap = liftM

-- instance Applicative EventMT where
--   pure = return
--   (<*>) = ap

-- instance Monad EventMT where
--   return x = Running x
--   m >>= g = case m of
--               EventMError -> EventMError
--               Running x   -> g x

-- newtype EventMT m a = EventMT { runEventMT :: m (EventM a)}

-- mapEventMT :: (m (EventM a) -> n (EventM b)) -> EventMT m a -> EventMT n b
-- mapEventMT f = EventMT . f . runEventMT

-- instance (Functor m) => Functor (EventMT m) where
--   fmap f = mapEventMT (fmap  (fmap f))

-- instance MonadTrans EventMT where
--   lift = EventMT . liftM

eventBoxCallback :: EventM EMotion Bool
eventBoxCallback = do
  (x,y) <- eventCoordinates
  liftIO $ print (x,y)
  return True

main = do
  initGUI
  builder <- builderNew
  currDir <- getCurrentDirectory
  builderAddFromFile builder (currDir ++ "/src/frontend/rectangle.glade")
  mainWindow <- builderGetObject builder castToWindow "main_window"
  evbox <- builderGetObject builder castToEventBox "event_box"
  -- setCallback mainWindow handleMotionEvent
  on evbox motionNotifyEvent eventBoxCallback

  -- mainWindow `onMotionNotify` handleMotionEvent
  widgetAddEvents evbox [PointerMotionMask]
  on mainWindow objectDestroy mainQuit
  widgetShowAll mainWindow
  mainGUI

main' = do
  initGUI
  home <- getHomeDirectory

  window1 <- windowNew

  -- Frames
  frame1 <- frameNew
  frame2 <- frameNew

  -- Windows
  window1 <- windowNew
  windowSetPosition window1 WinPosCenter
  set window1 [ containerBorderWidth := 10
              , windowTitle := "Hox"
              , windowResizable := True
              , windowDefaultWidth := 600
              , windowDefaultHeight := 300]

  sandBoxWindow <- windowNew
  windowSetPosition sandBoxWindow WinPosCenter
  set sandBoxWindow [ containerBorderWidth := 10
              , windowTitle := "Sandbox"
              , windowResizable := True
              , windowDefaultWidth := 600
              , windowDefaultHeight := 300]

  on window1 objectDestroy mainQuit
  on sandBoxWindow objectDestroy mainQuit

  -- Widgets
  entry <- entryNew
  fileChooser <- fileChooserButtonNew "Deck" FileChooserActionOpen
  button <- buttonNew
  set button [ buttonLabel := "Validate" ]
  on button buttonActivated $ do
    fn <- fileChooserGetFilename fileChooser
    case fn of
      Just x  -> do
        putStrLn $ "Validating " ++ x
        handle <- openFile x ReadMode
        contents <- hGetContents handle
        let d = deckList contents

        print $ "Deck size: " ++ show (sum (M.elems d))
        hClose handle
        if validDeck d
        then do
          widgetHide window1
          builder <- builderNew
          currDir <- getCurrentDirectory
          builderAddFromFile builder (currDir ++ "/src/gameUI.glade")
          sandBoxWindow <- builderGetObject builder castToWindow "sandbox_window"

          widgetShowAll sandBoxWindow
        else
          putStrLn "Invalid Deck"
      Nothing -> putStrLn "No file selected"

  on fileChooser fileChooserButtonFileSet $ do
    Just fn <- fileChooserGetFilename fileChooser
    putStrLn $ "Selected: " ++ fn

  -- Grids
  grid1 <- gridNew
  gridSetColumnSpacing grid1 10
  gridSetRowSpacing grid1 10
  gridSetColumnHomogeneous grid1 True

  gridAttach grid1 fileChooser 0 0 3 2
  gridAttach grid1 button 6 0 1 1

  -- Containers
  containerAdd window1 frame1
  containerAdd frame1 grid1

  -- set window1 [ windowTitle := "Hox", containerChild := button ]
  widgetShowAll window1

  mainGUI
