{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           CardList
import           ComplexTypes
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class               (liftIO)
import           Data.GI.Base.ManagedPtr              (castTo, unsafeCastTo)
import           Data.GI.Base.Signals
import qualified Data.Map                             as M
import           Data.Maybe
import qualified Data.Text                            as T
import           GI.Gtk                               (EventBox (..),
                                                       Label (..), Window (..),
                                                       widgetShowAll)
import qualified GI.Gtk                               as GI (init, main,
                                                             mainQuit)
import           GI.Gtk.Objects.Builder
import           GI.Gtk.Objects.EventControllerMotion
import           GI.Gtk.Objects.Label
-- import           Graphics.UI.Gtk
-- import           Graphics.UI.Gtk.Abstract.Widget
import           Graphics.UI.Gtk.Gdk.Events
import           Graphics.UI.Gtk.Layout.Fixed
import           Graphics.UI.Gtk.Misc.EventBox
import           Reactive.Banana
import           Reactive.Banana.Frameworks
import           System.Directory
import           System.IO
import           Text.Read                            hiding (get)
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

-- handleMotionEvent :: Event -> IO Bool
-- handleMotionEvent Motion {eventX = xMain, eventY = yMain, eventXRoot = xEvent, eventYRoot = yEvent} = do
--   -- (x, y) <- windowGetPosition mainWindow
--   print $ "Event at " ++ show xEvent ++ " " ++ show yEvent
--   return True
-- handleMotionEvent _ = do
--   print "Here"
--   return True

-- getMousePosition = do
--   events <- pollEvents
--   -- mousePos <- getAbsoluteMouseLocation
--   print events

-- type WidgetMotionNotifyEventCallback = EventMotion -> IO Bool

-- data EventM a = EventMError | Running a


makeSources = newAddHandler

-- Reader Monad --
-- newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

-- GTK --
-- type EventM t a = ReaderT (Ptr t) IO a
-- labelSetText :: self -> string -> IO ()
-- motionNotifyEvent :: Signal self (EventM EMotion Bool)

-- Reactive-Banana --
-- type Event a = [(Time, a)]
-- type Handler a = a -> IO ()
-- newtype AddHandler a = AddHandler { register :: Handler a -> IO (IO ())}
-- fromAddHandler :: AddHandler a -> MomentIO (Event a)
-- fromAddHandler :: a -> IO () -> IO (IO ()) -> MomentIO (Event a)

type EventSource a = (AddHandler a, a -> IO ())

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

data A = A {example1 :: Int}

main = do
  GI.init Nothing
  builder <- builderNew
  currDir <- getCurrentDirectory
  builderAddFromFile builder (T.pack (currDir ++ "/src/frontend/gameUI.glade"))
  mainWindow <- builderGetObject builder "main_window" >>= unsafeCastTo Window . fromJust

  evbox <- builderGetObject builder "event_box" >>= unsafeCastTo EventBox . fromJust
  -- prev:  on evbox motionNotifyEvent eventBoxCallback

  -- presumably now: on evbox ----- eventBoxCallback

  -- ecm <- eventControllerMotionNew
  -- on ecm #motion eventBoxCallback
  pName <- builderGetObject builder "p_name" >>= unsafeCastTo Label . fromJust
  labelSetText pName "test"

  pLife <- builderGetObject builder "p_life" >>= unsafeCastTo Label . fromJust
  pHand <- builderGetObject builder "p_hand" >>= unsafeCastTo Label . fromJust
  pLibrary <- builderGetObject builder "p_library" >>= unsafeCastTo Label . fromJust
  pGraveyard <- builderGetObject builder "p_graveyard" >>= unsafeCastTo Label . fromJust
  pExile <- builderGetObject builder "p_exile" >>= unsafeCastTo Label . fromJust
  pWhite <- builderGetObject builder "p_white" >>= unsafeCastTo Label . fromJust
  pBlue <- builderGetObject builder "p_blue" >>= unsafeCastTo Label . fromJust
  pBlack <- builderGetObject builder "p_black" >>= unsafeCastTo Label . fromJust
  pRed <- builderGetObject builder "p_red" >>= unsafeCastTo Label . fromJust
  pGreen <- builderGetObject builder "p_green" >>= unsafeCastTo Label . fromJust
  pColorless <- builderGetObject builder "p_colorless" >>= unsafeCastTo Label . fromJust

  -- mainWindow `onMotionNotify` handleMotionEvent
  -- widgetAddEvents evbox [ButtonPressMask, PointerMotionMask]
  -- on mainWindow (fromLabel @"destroy" :: String) GI.mainQuit

  on mainWindow #destroy GI.mainQuit
  -- on mainWindow objectDestroy mainQuit

  -- let networkDescription :: MomentIO()
  --     networkDescription = do
  --       emouse <- fromAddHandler mainWindow



  -- network <- compile networkDescription
  -- actuate network
  widgetShowAll mainWindow
  GI.main
  return ()

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
