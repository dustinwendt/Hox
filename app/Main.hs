-- # LANGUAGE OverloadedStrings #
{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Card
import           CardList
import           Colors
import           Control.Lens                hiding (set)
import           Control.Monad
import           Control.Monad.State
import qualified Data.Map                    as M
import           Graphics.UI.Gtk             hiding (Stack, get)
import           Graphics.UI.Gtk.Builder
import           Graphics.UI.Gtk.Layout.Grid
import           Player
import           System.Directory
import           System.IO
import           Text.Read                   hiding (get)
import           Util
import           Zones

type SId = String
type Id = String

initIds :: [Id]
initIds = concat [ replicateM k (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']) | k <- [1..]]

type Attack = [(Id, PId)]
type Defend = [(Id, Id)]
data Combat = Combat
  { _attackers :: Attack
  , _defenders :: Defend } deriving (Eq, Show)

noCombat :: Combat
noCombat = Combat
 { _attackers = []
 , _defenders = []
 }

data Phase = Untap | Upkeep | Draw | Main | BegCom | DecAttack Combat | DecBlock Combat
           | FirstDamCom Combat | DamCom Combat | EndCom | End | Cleanup deriving (Eq, Show)

combat :: [Phase]
combat = [BegCom, DecAttack noCombat, DecBlock noCombat, FirstDamCom noCombat, DamCom noCombat, EndCom]

phaseOrder :: [Phase]
phaseOrder = cycle [Untap, Upkeep, Draw, Main] ++ combat  ++ [Main, End, Cleanup]

data Occurrences = DrawFromEmpty PId

data GameState = GameState
 {
   _players      :: M.Map PId Player
 , _stack        :: Stack
 , _activePlayer :: PId
 , _exile        :: Exile
 , _battlefield  :: Battlefield
 , _phases       :: [Phase]
 , _turnOrder    :: [PId]
 , _turns        :: [PId]
 , _priority     :: Maybe PId
 , _stormCount   :: Int
 , _precombat    :: Bool
 , _occurrences  :: [Occurrences]
 , _passes       :: Int
 , _ids          :: [Id]
 }

$(makeLenses ''GameState)

data GameOutcome = DrawGame | Win PId

data PassSignal = NextPhase | Pass | Resolve deriving (Eq, Show)

type Game a = State GameState a

defaultGameState = GameState
  { _players      = M.empty
  , _stack        = []
  , _activePlayer = You
  , _exile        = []
  , _battlefield  = []
  , _phases       = []
  , _turnOrder    = []
  , _turns        = []
  , _stormCount   = 0
  , _priority     = Nothing
  , _precombat    = True
  , _occurrences  = []
  , _passes       = 0
  , _ids          = initIds
  }

idatN :: Int -> Game Id
idatN 0 = freshId
idatN n = do freshId
             idatN (n-1)

freshId :: Game Id
freshId = do
  g <- get
  let x = head $ g ^. ids
  modify $ over ids tail
  return x

-- newtype State s a = State { runState :: s -> (a, s) }

-- Beginning of step or phase
-- Turn-based actions occur
-- active player recieves priority
-- Player passed
-- Twice
-- Stack Empty
-- End of Turn

-- givePriority :: PId -> Game
-- givePriority pid = do

stateCheck = undefined

givePriority :: PId -> GameState -> GameState
givePriority pid g = do stateCheck

resolveStack :: GameState -> GameState
resolveStack = undefined

nextPlayer :: PId -> GameState -> PId
nextPlayer pid g = f pid (g ^. turnOrder) where
    f p (x:xs) | p == x    = head xs
               | otherwise = f p xs

advPhase = undefined
-- advPhase :: Game ()
-- advPhase = do
--   g <- get
--   let p = currentPhase g in
--   case p of
--     -- DecAttack a -> undefined
--     _ -> modify $ over phases tail

resolve :: Game ()
resolve = do undefined

currentPhase :: GameState -> Phase
currentPhase g = head $ g ^. phases

pass :: Game ()
pass = do
  modify $ over passes (+1)
  g <- get
  case g ^. priority of
    Just p -> if g ^. passes >= length (g ^. players)
              then if null $ g ^. stack
                   then advPhase
                   else resolve
              else pass
    Nothing -> pass


  -- let Just ap = do g ^. activePlayer
  --     p = g ^. priority
  -- -- Just p <- g ^. priority
  -- if g ^. passes == length (g ^. players)
  -- then if p == ap && null (g ^. stack)
  --      then return NextPhase
  --      else return Resolve
  -- else return Pass

  -- case g ^. priority of
  --   Just x -> undefined
  --   Nothing -> undefined
  -- return (g ^. passes + 1)

draw :: Int -> PId -> Game ()
draw n p = do
  g <- get
  let player = (g ^. players) M.! p
      lib = player ^. library
      updatedPlayer = over library (drop n) . over hand (++ take n lib) $ player
      u = over players (M.insert p updatedPlayer) in
    if n > length lib
    then modify $ u . over occurrences (DrawFromEmpty p :)
    else modify u

-- pass :: GameState -> GameState
-- pass g = let g'  = over passes (+1) g in
--     case g ^. priority of
--     Just x -> if x == (g ^. activePlayer) && (g ^. passes) == length (keys (g ^. players))
--               then if null (g ^. stack)
--                    then advancePhase g
--                    else resolveStack g
--               else givePriority (nextPlayer x g) g'
--     Nothing -> advancePhase g

-- state :: (s -> (a,s)) -> State s a
-- state = State

-- instance Functor (State s) where
--   fmap = liftM

-- instance Applicative (State s) where
--   pure = return
--   (<*>) = ap

-- instance Monad (State s) where
--   return x = state (\s -> (x,s))
--   p >>= k = state $ \ s0 ->
--     let (x, s1) = runState p s0
--     in runState (k x) s1

play :: GameState -> GameState
play _ = undefined

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

main = do
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
