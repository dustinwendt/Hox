{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module Game where

import           CardList
import           ComplexTypes
import           Data.GI.Base.ManagedPtr
import           Data.GI.Base.Signals
import qualified Data.Map                as M
import           Data.Maybe
import qualified Data.Text               as T
import           FRP.Yampa
import           GI.Gtk                  (EventBox (..), Label (..),
                                          Window (..))
import qualified GI.Gtk                  as GI (init, main, mainQuit)
import           GI.Gtk.Objects.Builder
import           GI.Gtk.Objects.Label
import           GI.Gtk.Objects.Widget
import           SDL.Input.Mouse
import           System.Directory
import           Util

deckList :: String -> M.Map GameObject Int
deckList s = foldr (uncurry (M.insertWith (+)) . pLine) M.empty (lines s)
  where
    pLine s = let (a,b) = break isSpace s in
            (strToCard (stripCard b), read a :: Int)

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


buildUI = do
  GI.init Nothing
  builder <- builderNew
  currDir <- getCurrentDirectory
  builderAddFromFile builder (T.pack (currDir ++ "/src/frontend/gameUI.glade"))
  mainWindow <- builderGetObject builder "main_window" >>= unsafeCastTo Window . fromJust

  evbox <- builderGetObject builder "event_box" >>= unsafeCastTo EventBox . fromJust

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

  on mainWindow #destroy GI.mainQuit

  #showAll mainWindow
  GI.main

