module Main where

import Control.Monad
import Data.Maybe
import Lib
import Card
import Player

type Spell = Card

data Phase = Untap | Upkeep | Draw | Main1 | Combat | Main2 | End

data GameState = GameState
                 { zones     :: [Player]
                 , stack     :: [Spell]
                 , turn      :: Int
                 , phase     :: Phase
                 , turnOrder :: [Int]
                 , priority  :: Int
                 }

nextPhase :: Phase -> Phase
nextPhase Untap = Upkeep
nextPhase Upkeep = Draw
nextPhase Draw = Main1
nextPhase Main1 = Combat
nextPhase Combat = Main2
nextPhase Main2 = End
nextPhase End = Untap

pass :: GameState -> GameState
pass s = case s of
  GameState {} = GameState 

data TurnstileState = Locked | Unlocked
  deriving (Eq, Show)

data TurnstileOutput = Thank | Open | Tut
  deriving (Eq, Show)

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

coin, push :: TurnstileState -> (TurnstileOutput, TurnstileState)
coin _ = (Thank, Unlocked)

coinS, pushS :: State TurnstileState TurnstileOutput
coinS = state coin
pushS = state push

push Unlocked = (Open, Locked)
push Locked = (Tut, Locked)

monday :: TurnstileState -> ([TurnstileOutput], TurnstileState)
monday s0 =
  let (a1, s1) = coin s0
      (a2, s2) = push s1
      (a3, s3) = push s2
      (a4, s4) = coin s3
      (a5, s5) = push s4
   in ([a1, a2, a3, a4, a5], s5)

mondayS :: State TurnstileState [TurnstileOutput]
mondayS = sequence [coinS, pushS, pushS, coinS, pushS]

regularPersonS, distractedPersonS, hastyPersonS :: State TurnstileState [TurnstileOutput]

regularPersonS = sequence [coinS, pushS]

distractedPersonS = sequence [coinS]

hastyPersonS = do a1 <- pushS
                  case a1 of
                    Open -> return [a1]
                    _     -> do as <- sequence [coinS, pushS]
                                return (a1:as)

luckyPairS :: Bool -> State TurnstileState Bool
luckyPairS b = do
  if b then distractedPersonS else regularPersonS
  o <- pushS
  return (o == Open)

regularPerson, distractedPerson, hastyPerson :: TurnstileState -> ([TurnstileOutput], TurnstileState)

regularPerson s0 =
  let (a1, s1) = coin s0
      (a2, s2) = push s1
  in ([a1, a2], s2)

distractedPerson s0 =
  let (a1, s1) = coin s0
  in ([a1], s1)

hastyPerson Unlocked = let (a1, s1) = push Unlocked in
                         ([a1], s1)
hastyPerson Locked   = let (a1, s1) = push Locked
                           (a2, s2) = coin s1
                           (a3, s3) = push s2 in
                         ([a1,a2,a3], s3)

tuesday :: TurnstileState -> ([TurnstileOutput], TurnstileState)
tuesday s0 = let (a, s1) = regularPerson s0
                 (b, s2) = hastyPerson s1
                 (c, s3) = distractedPerson s2
                 (d, s4) = hastyPerson s3
             in (a ++ b ++ c ++ d, s4)

luckyPair :: Bool -> TurnstileState -> (Bool, TurnstileState)
luckyPair True s0 = let (a, s1) = distractedPerson s0
                        (b, s2) = push s1 in
                      (True, s2)
luckyPair False s0 = let (a, s1) = regularPerson s0
                         (b, s2) = push s1 in
                       (False, s2)


-- turn : Monad Magic
-- turn = do
--   untap
--   upkeep
--   draw
--   main
--   combat
--   main
--   end

main :: IO ()
main = putStrLn "Here"
