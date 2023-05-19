{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Game where

import           CardList
import           ComplexTypes
import           Control.Arrow
import           Control.Lens
import           Control.Monad
import           Data.GI.Base.BasicTypes
import           Data.GI.Base.ManagedPtr
import           Data.GI.Base.Signals
import qualified Data.Map                             as M
import           Data.Maybe
import qualified Data.Text                            as T
import           Foreign.C
import           GHC.Word
import           GI.Gdk.Flags
import           GI.Gdk.Structs.EventKey
import           GI.Gtk                               (EventBox (..),
                                                       Label (..),
                                                       SignalProxy (..),
                                                       Window (..), on)
import qualified GI.Gtk                               as GI (init, main,
                                                             mainQuit)
import           GI.Gtk.Objects.Builder
import           GI.Gtk.Objects.Button
import           GI.Gtk.Objects.EventControllerKey
import           GI.Gtk.Objects.EventControllerMotion
import           GI.Gtk.Objects.Label
import           GI.Gtk.Objects.Widget
import           Gtk
import           Reactive.Banana
import           Reactive.Banana.Frameworks
import           SDL.Input.Mouse
import           SDL.Vect
import           System.Directory
import           System.Random
import           Util
-- import System.Glib.Signals (on)

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

type Coord = (Double, Double)
data Input = Input { mouseLoc :: Coord, leftClicked :: Bool} deriving Show

-- labelUpdateSF :: SF (Event, Point)
-- labelUpdateSF = proc (event, point) -> do
--   undefined

-- echo = do
--   mb <- getMouseButtons
--   P (V2 x y) <- getAbsoluteMouseLocation -- >>= \v -> return (fromEnum $ v ^. SDL._x, fromEnum)
--   print (x,y)
--   -- when (mb ButtonLeft) (putStrLn loc)
--   echo

-- data Prop' w = forall a. (Attr w a) :== (Behavior a)
-- sink :: w -> [Prop' w] -> MomentIO ()

-- initGame :: Game ()
-- initGame

getMousePos :: IO (Double, Double)
getMousePos = do
  P (V2 x y) <- getAbsoluteMouseLocation
  return (fromIntegral x, fromIntegral y)

wrapHaskellCallbackType :: HaskellCallbackType WidgetKeyPressEventSignalInfo
wrapHaskellCallbackType = undefined

networkDescription' :: MomentIO ()
networkDescription' = do
  b <- builderNew
  currDir <- liftIO getCurrentDirectory
  builderAddFromFile b $ T.pack $ currDir ++ "/src/frontend/gameUI.glade"

  mainWindow <- castB b "main_window" Window
  evbox <- castB b "event_box" EventBox
  pName <- castB b "p_name" Label
  pGraveyard <- castB b "p_graveyard" Label
  pGraveyardButton <- castB b "p_graveyard_button" Button
  pExile <- castB b "p_exile" Label
  pExileButton <- castB b "p_exile_button" Button

  destroyE <- signalE0 mainWindow #destroy
  reactimate $ GI.mainQuit <$ destroyE

  graveyardClickedE <- signalE0 pGraveyardButton #clicked
  graveyardClickCount <- accumB 0 ((+1) <$ graveyardClickedE)

  exileClickedE <- signalE0 pExileButton #clicked
  exileClickCount <- accumB 0 ((+1) <$ exileClickedE)

  sink pGraveyard [#label :== showT <$> graveyardClickCount]
  sink pExile [#label :== showT <$> exileClickCount]

buildGUIFromFile :: String -> IO GUI
buildGUIFromFile fp = do
  b <- builderNew
  currDir <- getCurrentDirectory
  builderAddFromFile b $ T.pack $ currDir ++ fp
  mWindow <- castB b "main_window" Window
  eBox <- castB b "event_box" EventBox
  pl <- castB b "p_life" Label
  pg <- castB b "p_graveyard" Label
  pe <- castB b "p_exile" Label
  pw <- castB b "p_white" Label
  pu <- castB b "p_blue" Label
  pb <- castB b "p_black" Label
  pr <- castB b "p_red" Label
  pg <- castB b "p_green" Label
  pc <- castB b "p_colorless" Label
  pgb <- castB b "p_graveyard_button" Button
  peb <- castB b "p_exile_button" Button
  esb <- castB b "end_step_button" Button
  msb <- castB b "main_step_button" Button
  stb <- castB b "self_turn_button" Button
  peb <- castB b "prior_end_button" Button
  cb <- castB b "concede_button" Button
  ntb <- castB b "next_turn_button" Button

  on mWindow #destroy GI.mainQuit

  #addEvents mWindow [EventMaskButtonMotionMask,
                         EventMaskButtonPressMask,
                         EventMaskButtonReleaseMask,
                         EventMaskKeyPressMask]

  return GUI {
    _mainWindow = mWindow
    , _evbox = eBox
    , _pLife = pl
    , _pGraveyard = pg
    , _pExile = pe
    , _pWhite = pw
    , _pBlue = pu
    , _pBlack = pb
    , _pRed = pr
    , _pGreen = pg
    , _pColorless = pc
    , _pGraveyardButton = pgb
    , _pExileButton = peb
             }

setUpNetwork :: GUI -> MomentIO ()
setUpNetwork gui = do
  -- destroyE <- signalE0 mainWindow #destroy
  -- reactimate $ GI.mainQuit <$ destroyE

  graveyardClickedE <- signalE0 (gui ^. pGraveyardButton) #clicked
  graveyardClickCount <- accumB 0 ((+1) <$ graveyardClickedE)

  exileClickedE <- signalE0 (gui ^. pExileButton) #clicked
  exileClickCount <- accumB 0 ((+1) <$ exileClickedE)

  sink (gui ^. pGraveyard) [#label :== showT <$> graveyardClickCount]
  sink (gui ^. pExile) [#label :== showT <$> exileClickCount]

gl :: IO ()
gl = do
  GI.init Nothing
  gui <- buildGUIFromFile "/src/frontend/gameUI.glade"
  let networkDescription :: MomentIO ()
      networkDescription = do
       graveyardClickedE <- signalE0 (gui ^. pGraveyardButton) #clicked
       graveyardClickCount <- accumB 0 ((+1) <$ graveyardClickedE)
       sink (gui ^. pGraveyard) [#label :== showT <$> graveyardClickCount]

  network <- compile networkDescription
  #showAll (gui ^. mainWindow)
  GI.main

gameLoop :: IO ()
gameLoop = do
  GI.init Nothing
  b <- builderNew
  currDir <- getCurrentDirectory
  builderAddFromFile b $ T.pack $ currDir ++ "/src/frontend/gameUI.glade"

  mainWindow <- castB b "main_window" Window
  evbox <- castB b "event_box" EventBox

  pName <- castB b "p_name" Label
  labelSetText pName "test"

  pLife <- castB b "p_life" Label
  pHand <- castB b "p_hand" Label
  pLibrary <- castB b "p_library" Label
  pGraveyard <- castB b "p_graveyard" Label
  pGraveyardButton <- castB b "p_graveyard_button" Button
  pExile <- castB b "p_exile" Label
  pExileButton <- castB b "p_exile_button" Button
  pWhite <- castB b "p_white" Label
  pBlue <- castB b "p_blue" Label
  pBlack <- castB b "p_black" Label
  pRed <- castB b "p_red" Label
  pGreen <- castB b "p_green" Label
  pColorless <- castB b "p_colorless" Label

  endStepButton <- castB b "end_step_button" Button
  mainStepButton <- castB b "main_step_button" Button
  selfTurnButton <- castB b "self_turn_button" Button
  priorEndButton <- castB b "prior_end_button" Button
  concedeButton <- castB b "concede_button" Button
  nextTurnButton <- castB b "next_turn_button" Button
  --on mainWindow #destroy GI.mainQuit

  #addEvents mainWindow [EventMaskButtonMotionMask,
                         EventMaskButtonPressMask,
                         EventMaskButtonReleaseMask,
                         EventMaskKeyPressMask]




  let motionCallback :: EventControllerMotionMotionCallback
      motionCallback x y = do
        mb <- getMouseButtons

        labelSetText pName (showT (x,y))

        let bs = [ButtonLeft, ButtonMiddle, ButtonRight, ButtonX1, ButtonX2]

        print $ show (map mb bs) ++ ": " ++ show (x,y)

        when (mb ButtonLeft) $ do
          putStrLn "Button held down"
          labelSetText pName (showT (x,y))

  let keyPressCallback :: EventKey -> IO Bool
      keyPressCallback k = do
        w <- getEventKeyKeyval k
        print $ "Key pressed: " ++ show w
        case w of
          65473 -> do return True
          -- GDK_KEY_F4 -> return True
          _     -> return False

   -- Defined in Data.GI.Base.Signals
  -- on :: (Data.GI.Base.BasicTypes.GObject object, MonadIO m,
  --        Data.GI.Base.Signals.SignalInfo info) =>
  --        object
  --        -> SignalProxy object info
  --        -> Data.GI.Base.Signals.HaskellCallbackType info
  --        -> m Data.GI.Base.Signals.SignalHandlerId

  -- Defined in System.GLib.Signals
  -- on :: object -> Signal object callback -> callback -> IO (ConnectId object)


  -- _ <- on evbox WidgetButtonPressEventSignalInfo $ do
  --   button <- eventButton
  --   case button of
  --     LeftButton -> do
  --       (x,y) <- eventCoordinates
  --       labelSetText pName (T.pack (show (x,y)))
  --     _ -> undefined

  -- eck <- eventContro
  ecm <- eventControllerMotionNew evbox
  eck <- eventControllerKeyNew mainWindow
  on ecm #motion motionCallback

  on mainWindow #keyPressEvent keyPressCallback

  let buttonCallback :: ButtonClickedCallback
      buttonCallback = do
        print "Graveyard button clicked"

  on pGraveyardButton #clicked buttonCallback

  let networkDescription :: MomentIO ()
      networkDescription = mdo
        -- (clickHandler, fireClick) <- liftIO newAddHandler
        -- (colorlessHandler, fireColorless) <- liftIO newAddHandler
        -- -- register lifeHandler putStrLn
        -- emouse <- fromAddHandler clickHandler

        -- testMotion <- fromChanges emouse
        destroyE <- signalE0 mainWindow #destroy
        reactimate $ GI.mainQuit <$ destroyE

        graveyardClickedE <- signalE0 pGraveyardButton #clicked
        graveyardClickCount <- accumB 0 ((+1) <$ graveyardClickedE)

        exileClickedE <- signalE0 pExileButton #clicked
        exileClickCount <- accumB 0 ((+1) <$ exileClickedE)

        sink pGraveyard [#label :== showT <$> graveyardClickCount]
        sink pExile [#label :== showT <$> exileClickCount]

  network <- compile networkDescription

  -- -- Starts event network
  actuate network

  #showAll mainWindow

  GI.main


