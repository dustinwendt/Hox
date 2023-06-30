{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Game where

import           CardList
import           Colors
import           ComplexTypes
import           Control.Arrow
import           Control.Lens                         hiding (view)
import           Control.Monad
import           Control.Monad.State.Lazy
import           Data.GI.Base.BasicTypes
import           Data.GI.Base.ManagedPtr
import           Data.GI.Base.Signals
import qualified Data.Map                             as M
import           Data.Maybe
import qualified Data.Text                            as T
import           Data.Text.Internal.Unsafe.Char
import           GI.Gdk.Flags
import           GI.Gdk.Structs.EventKey
import           GI.Gtk                               (Box (..), EventBox (..),
                                                       Label (..),
                                                       SignalProxy (..),
                                                       Window (..), on)
import qualified GI.Gtk                               as GI (init, main,
                                                             mainQuit)
import qualified GI.Gtk.Declarative                   as Gtkd
import           GI.Gtk.Declarative.App.Simple
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

updateLabels :: GameState -> IO ()
updateLabels s = do
  let g = s ^. gui
      u = M.findWithDefault (defaultPlayer You) You (s ^. players)
      mp = (u ^. manaPool) in
    do labelSetText (g ^. pLife) (showT (u ^. life))
       labelSetText (g ^. pHand) (showT (length (u ^. hand)))
       labelSetText (g ^. pLibrary) (showT (length (u ^. library)))
       labelSetText (g ^. pGraveyard) (showT (length (u ^. graveyard)))
       labelSetText (g ^. pExile) (showT (length (s ^. exile)))
       labelSetText (g ^. activePlayerLabel) (showT ("Active player: " ++ show (s ^. activePlayer)))
       labelSetText (g ^. phaseLabel) (showT ("Phase: " ++ show (head (s ^. phases))))
       labelSetText (g ^. pWhite) (showT (M.findWithDefault 0 (Colored White) mp))
       labelSetText (g ^. pBlue) (showT (M.findWithDefault 0 (Colored Blue) mp))
       labelSetText (g ^. pBlack) (showT (M.findWithDefault 0 (Colored Black) mp))
       labelSetText (g ^. pRed) (showT (M.findWithDefault 0 (Colored Red) mp))
       labelSetText (g ^. pGreen) (showT (M.findWithDefault 0 (Colored Green) mp))
       labelSetText (g ^. pColorless) (showT (M.findWithDefault 0 Colorless mp))

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
  ph <- castB b "p_hand" Label
  plib <- castB b "p_library" Label
  pg <- castB b "p_graveyard" Label
  pe <- castB b "p_exile" Label
  pw <- castB b "p_white" Label
  pu <- castB b "p_blue" Label
  pb <- castB b "p_black" Label
  pr <- castB b "p_red" Label
  pg <- castB b "p_green" Label
  pc <- castB b "p_colorless" Label
  gyl <- castB b "p_graveyard_label" Label
  pgb <- castB b "p_graveyard_button" Button
  pel <- castB b "p_exile_label" Label
  peb <- castB b "p_exile_button" Button
  esb <- castB b "end_step_button" Button
  msb <- castB b "main_step_button" Button
  stb <- castB b "self_turn_button" Button
  peb <- castB b "prior_end_button" Button
  cb <- castB b "concede_button" Button
  ntb <- castB b "next_turn_button" Button
  passb  <- castB b "pass_button" Button
  activep <- castB b "active_player_label" Label
  passLabel <- castB b "passes_label" Label
  phLabel <- castB b "phase_label" Label

  on mWindow #destroy GI.mainQuit

  #addEvents mWindow [EventMaskButtonMotionMask,
                         EventMaskButtonPressMask,
                         EventMaskButtonReleaseMask,
                         EventMaskKeyPressMask]

  return GUI {
    _mainWindow = mWindow
    , _evbox = eBox
    , _pLife = pl
    , _pHand = ph
    , _pLibrary = plib
    , _pGraveyard = pg
    , _pExile = pe
    , _pWhite = pw
    , _pBlue = pu
    , _pBlack = pb
    , _pRed = pr
    , _pGreen = pg
    , _pColorless = pc
    , _pGraveyardLabel = gyl
    , _pGraveyardButton = pgb
    , _pExileLabel = pel
    , _pExileButton = peb
    , _passButton  = passb
    , _activePlayerLabel = activep
    , _passesLabel = passLabel
    , _phaseLabel = phLabel
             }

-- buildGame :: IO (GameState)
buildGame = do
  GI.init Nothing
  gui <- buildGUIFromFile "/src/frontend/gameUI.glade"
  updateLabels $ defaultGameState { _gui = gui}
  #addEvents (gui ^. mainWindow) [EventMaskButtonMotionMask,
                                  EventMaskButtonPressMask,
                                  EventMaskButtonReleaseMask,
                                  EventMaskKeyPressMask]
  -- let passCallback :: Game ()
  --     passCallback = do
  --       print "Pass button pressed"
        -- do g <- get
        --    pass

  -- on (gui ^. passButton) #clicked passCallback
  on (gui ^. mainWindow) #destroy GI.mainQuit
        -- updateLabels
  #showAll (gui ^. mainWindow)
  -- GI.main -- IO ()
  -- print "Here"
  -- return (defaultGameState :: GameState)

-- runGame :: Game ()
-- runGame = do


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

data Result = Pass  | Closed

main' = void $ run App
  { view = view'
  , update = update'
  , inputs = []
  , initialState = defaultGameState }

view' :: GameState -> AppView Window Result
view' = undefined


-- view' s = Gtkd.bin
--   Window
--   [#title Gtkd.:= "Hox", Gtkd.on #deleteEvent (const (True, Closed))]
--   (Gtkd.container Box
--         [])

update' = undefined
-- update' s e = case e of
--   Closed -> Exit
--   _      -> Exit

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
  passButton     <- castB b "pass_button" Button
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
        print $ "Key pressed: " ++ show (unsafeChr32 w)
        case w of
          65473 -> do
            print "f4"
            return True
          -- GDK_KEY_F4 -> return True
          65474 -> do return True
          65476 -> do return True
          65478 -> do return True
          65480 -> do return True
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


