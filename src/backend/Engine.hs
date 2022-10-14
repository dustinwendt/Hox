{-# LANGUAGE TemplateHaskell #-}
module Engine where

import           Card
import           CardList
import           Colors
import           Control.Lens
import           Control.Monad.State
import qualified Data.Map            as M
import           Player
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

data Phase = Untap | Upkeep | Draw | PreCombatMain | BegCom | DecAttack Combat | DecBlock Combat
           | FirstDamCom Combat | DamCom Combat | EndCom | PostCombatMain | End | Cleanup deriving (Eq, Show)


combat :: [Phase]
combat = [BegCom, DecAttack noCombat, DecBlock noCombat, FirstDamCom noCombat, DamCom noCombat, EndCom]

phaseOrder :: [Phase]
phaseOrder = cycle [Untap, Upkeep, Draw, PreCombatMain] ++ combat  ++ [PostCombatMain, End, Cleanup]

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

-- 104.1
data GameOutcome = DrawGame | Win PId | Restart

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

funCard :: GameObject -> Game ()
funCard ancestrallRecall = undefined

-- idatN :: Int -> Game Id
-- idatN 0 = freshId
-- idatN n = do freshId
--              idatN (n-1)

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

-- State based actions
-- 704.5a Player has 0 or less life. They lose
-- 704.5b Draw a card from an empty library. They lose
-- 704.5c 10 or more poison counters. They lose
-- 704.5d Token is in a zone other than the battlefield. Ceases to exist
-- 704.5e Copy of a spell is in any zone other than the stack. Ceases to exist
-- 704.5f Creature has 0 or less toughness. Goes to graveyard. Regeneration can't replace
-- 704.5g (>0) toughness and damage exceeds it. Goes to graveyard
-- 704.5h Has been dealt damage by deathtouch since the last statecheck. Goes to graeyard Regeneration can replace
-- 704.5i Planeswalker has 0 loyalty. Goes to graveyard
-- 704.5j Two or more legendary permanents of the same name are controlled by the same player. All but one go to graveyard
-- 704.5k If two or more permanents have the supertype World. Only the newest doesn't go to graveyard
-- 704.5m If an aura is attached to an illegal object or player or not attached to anything. Goes to graveyard
-- 704.5n If an equipment is attached to an illegal target. It becomes unattached. Still on the battlefield

-- A monad is a monoid in the category of endofunctors

leave :: PId -> Game ()
leave p = do
  modify $ over players (M.delete p)
  modify $ over stack (filter (\x -> x ^. owner /= p))
  modify $ over exile (filter (\x -> x ^. owner /= p))
  modify $ over battlefield (filter (\x -> x ^. owner /= p))
  modify $ over turnOrder (filter (/= p))
  modify $ over turns (filter (/= p))

stateCheck = undefined

givePriority :: PId -> Game ()
givePriority p = do
  stateCheck
  modify $ set priority (Just p)

-- resolveStack :: Game ()
-- resolveStack = do
--   g <- get
--   let c = head $ g ^. stack in
  -- modify $ over stack tail


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

doNothing :: Game ()
doNothing = return ()

untapPhaseAction :: PId -> Game ()
untapPhaseAction p = do
  g <- get
  modify $ over battlefield $ id

  -- (\x -> if x ^. controller == p
                                   -- then do x
                                      -- set sick False
                                   -- else x)

defaultPhaseActions :: PId -> [(Phase, Game ())]
defaultPhaseActions p = [(Untap, untapPhaseAction p),
                  (Upkeep, doNothing),
                  (Draw, draw 1 p),
                  (PreCombatMain, doNothing),
                  (BegCom, doNothing),
                  (DecAttack noCombat, doNothing),
                  (DecBlock noCombat, doNothing),
                  (EndCom, doNothing),
                  (PostCombatMain, doNothing),
                  (End, doNothing),
                  (Cleanup, doNothing)]

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

