{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module ComplexTypes where

import           Colors
import           Control.Exception
import           Control.Lens
import           Control.Monad.State
import qualified Data.Map            as M
import           GHC.Generics
import           Types

-- 208 Power/Toughness
data PT = Star | StarPlus Int | PT Int deriving (Eq)

-- 400.1
data Zones = Library | Hand | Battlefield | Graveyard | Stack | Exile | Command

type SId = String
type Id = String

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

data Keyword = Banding | Defender | FirstStrike | Fear | Flying | Haste | Indestructible | LandWalk | Protection | Reach | Regeneration | Trample | Vigilance deriving (Enum, Eq, Generic, Show)

data Occurrences = DrawFromEmpty PId


-- 109.3
data Properties = Properties
  { _name       :: String
  , _manaCost   :: Maybe [Pip]
  , _color      :: [Color]
  , _identity   :: [Color]
  , _keywords   :: [Keyword]
  , _typeLine   :: TypeLine
  , _oracleText :: String
  , _power      :: Maybe PT
  , _toughness  :: Maybe PT
  , _loyalty    :: Maybe Int
  , _function   :: Game ()
  }

instance Eq Properties where
  p1 == p2 = _name p1 == _name p2
          && _manaCost p1 == _manaCost p2
          && _color p1 == _color p2
          && _identity p1 == _identity p2
          && _keywords p1 == _keywords p2
          && _typeLine p1 == _typeLine p2
          && _oracleText p1 == _oracleText p2
          && _power p1 == _power p2
          && _toughness p1 == _toughness p2
          && _loyalty p1 == _loyalty p2

-- 110.5
data Status =
  Status { _tapped  :: Bool
         , _flipped :: Bool
         , _faceUp  :: Bool
         , _phased  :: Bool
         , _sick    :: Bool} deriving (Eq, Show)

-- 109.1
-- Token | Copy are represented by Nothing being passed to Permanent and Spell respectively
data ObjectType = Ability | Card | Spell (Maybe GameObject) | Permanent (Maybe GameObject) Status | Emblem deriving Eq

data GameObject =
  GameObject { _properties :: Properties
             , _objType    :: ObjectType
             , _owner      :: PId
             , _controller :: PId} deriving Eq

type Library = [GameObject]
type Hand = [GameObject]
type Battlefield = [GameObject]
type Graveyard = [GameObject]
type Stack = [GameObject]
type Exile = [GameObject]
type Command = [GameObject]

data Player = Player { _life        :: Int
                     , _library     :: Library
                     , _hand        :: Hand
                     , _graveyard   :: Graveyard
                     , _manaPool    :: ManaPool
                     , _maxHandSize :: Int
                     , _landsPlayed :: Int
                     , _maxLand     :: Int
                     }

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

type Game a = State GameState a

$(makeLenses ''Properties)

-- instance Eq Properties where
--   p1 == p2 = all [ p1 ^. x == p2 ^. x | x <- [name, manaCost, color] ]

-- 104.1
data GameOutcome = DrawGame | Win PId | Restart

data PassSignal = NextPhase | Pass | Resolve deriving (Eq, Show)

$(makeLenses ''GameObject)
$(makeLenses ''GameState)
$(makeLenses ''Player)

$(makeLenses ''Status)

instance Show PT where
  show Star         = "*"
  show (StarPlus i) = "*+" ++ show i
  show (PT i)       = show i

instance Show Properties where
  show p = show (p^.name) ++ " " ++ mc (p^.manaCost) ++ "\n" ++
           tl (p^.typeLine) ++
           tb (p^.oracleText) ++
           f (p^.power, p^.toughness)
   where f (Just x, Just y) = show x ++ "/" ++ show y ++ "\n"
         f _                = ""
         g []     = ""
         g (x:xs) = "{" ++ show x ++ "}" ++ g xs
         tl (TypeLine [] [] []) = ""
         tl typeline            = show typeline ++ "\n"
         tb textbox = if null textbox then "" else textbox ++ "\n"
         mc Nothing  = ""
         mc (Just p) = g p

instance Ord GameObject where
  a <= b = a ^. (properties . name) <= b ^. (properties . name)

instance Show GameObject where
  show o = show $ o^.properties

-- 110.5b
defaultStatus =
  Status { _tapped = False
         , _flipped = False
         , _faceUp = True
         , _phased = False
         , _sick = True }

isCard o = o^.objType == Card

isSpell o = case o^.objType of
  Spell _ -> True
  _       -> False

copySpell :: GameObject -> GameObject
copySpell s = assert (isSpell s) $ objType .~ Spell Nothing $ s

-- defaultLegality = Legality { standard = True
--                            , modern   = True
--                            , legacy   = True
--                            , vintage  = True
--                            }


defaultProperties :: Properties
defaultProperties = Properties
  { _name = "DefaultCard"
  , _manaCost = Nothing
  , _color = []
  , _identity = []
  , _keywords = []
  , _typeLine = TypeLine [] [] []
  , _oracleText = ""
  , _power = Nothing
  , _toughness = Nothing
  , _loyalty = Nothing
  , _function = doNothing
  }

defaultCard =
  GameObject { _properties = defaultProperties
             , _objType = Card
             , _owner = You
             , _controller = You}

-- 202.3
pipValue :: Pip -> Int
pipValue x = case x of
              HyPip a b -> if v a > v b then v a else v b
              p         -> v p
           where v XSym       = 0
                 v (GenSym i) = i
                 v _          = 1

manaValue :: GameObject -> Int
manaValue o = case o^.properties.manaCost of
  Just pips -> foldr ((+) . pipValue) 0 pips

isMonoColored :: GameObject -> Bool
isMonoColored o = length (o^.properties.color) == 1

isMultiColored :: GameObject -> Bool
isMultiColored o = length (o^.properties.color) > 1

isColorless :: GameObject -> Bool
isColorless o = null (o^.properties.color)



initIds :: [Id]
initIds = concat [ replicateM k (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']) | k <- [1..]]



combat :: [Phase]
combat = [BegCom, DecAttack noCombat, DecBlock noCombat, FirstDamCom noCombat, DamCom noCombat, EndCom]

phaseOrder :: [Phase]
phaseOrder = cycle [Untap, Upkeep, Draw, PreCombatMain] ++ combat  ++ [PostCombatMain, End, Cleanup]




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
