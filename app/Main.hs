{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Card
import           Colors
import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Data.Map      hiding (drop, map, null, take)
import           Lib
import           Player
-- import           Supply
-- import           System.Random
import           Text.Read hiding (get)
import           Zones

type SId = String
type Id = String

initIds = concat [ replicateM k (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']) | k <- [1..]]

-- runSupplyVars x = runSupply x vars
--     where vars = [replicate k ['a'..'z'] | k <- [1..]] >>= sequence

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
   _players      :: Map PId Player
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
  { _players      = empty
  , _stack        = []
  , _activePlayer = -1
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
  -- return $ head $ g ^. ids
  -- let xs = g ^. ids in
  --   modify $ over ids xs
  --   return $ head xs

-- type Game = State GameState GameOutput
-- pass :: GameState -> GameState
-- pass s = case s of
--   GameState {} -> GameState {}

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
  let player = (g ^. players) ! p
      lib = player ^. library
      updatedPlayer = over library (drop n) . over hand (++ take n lib) $ player
      u = over players (insert p updatedPlayer) in
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

-- draw :: Int -> PId -> GameState -> GameState
-- draw n p g =
--       -- Map PId Player
--   let ps = (g ^. players)
--       -- Player
--       player  = ps ! p
--       -- Library
--       l = player ^. library
--       -- Hand
--       h = player ^. hand
--       -- Hand
--       newHand = h ++ take n l
--       -- Library
--       newLib  = drop 3 l
--       -- Player
--       p' = (hand .~ newHand) . (library .~ newLib) $ player
--       ps' = insert p p' ps
--       g'  = (players .~ ps') g in
--     if n <= length l
--     then g'
--     else over occurrences (DrawFromEmpty p:) g'

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
  case x of
    Just x  -> return x
    Nothing -> formatPrompt

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

main :: IO ()
main = do
  f <- formatPrompt
  numPlayers <- playersPrompt
  putStrLn "Done"
