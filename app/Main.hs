module Main where

import Control.Monad
import Data.Char
import Data.Maybe
import Text.Read
import Lib
import Card
import Player
import Zones

data Phase = Untap | Upkeep | Draw | Main | BegCom | DecAttack | DecBlock | DamCom | EndCom | End | Cleanup deriving (Eq, Show)

combat = [BegCom, DecAttack, DecBlock, DamCom, EndCom]
phaseOrder = cycle [Untap, Upkeep, Draw, Main] ++ combat  ++ [Main, End, Cleanup]

data GameState = GameState
                 { zones     :: [Player]
                 , stack     :: Stack
                 , turn      :: PId
                 , phase     :: Phase
                 , nextPhases :: [Phase]
                 , turnOrder :: [Int]
                 , priority  :: Int
                 , precombat :: Bool
                 , landPlayed :: Bool
                 }

-- initialGameState = GameState
--   { stack = []
--   , nextPhases = phaseOrder
--   , precombat  = True
--   , landPlayed = False
--   }

-- pass :: GameState -> GameState
-- pass s = case s of
--   GameState {} -> GameState {}

newtype State s a = State { runState :: s -> (a, s) }

state :: (s -> (a,s)) -> State s a
state = State

instance Functor (State s) where
  fmap = liftM

instance Applicative (State s) where
  pure = return
  (<*>) = ap

instance Monad (State s) where
  return x = state (\s -> (x,s))
  p >>= k = state $ \ s0 ->
    let (x, s1) = runState p s0
    in runState (k x) s1

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
    Just x -> return x
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
