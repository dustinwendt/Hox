{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Engine where

import Colors
import Control.Monad.State
import qualified Data.Map as M
import Player
import Zones

type SId = String
type Id = String

type Attack = [(Id, PId)]
type Defend = [(Id, Id)]

data Combat = Combat { _attackers :: Attack, _defenders :: Defend }

data Phase = Untap | Upkeep | Draw | PreCombatMain | BegCom | DecAttack Combat | DecBlock Combat
           | FirstDamCom Combat | DamCom Combat | EndCom | PostCombatMain | End | Cleanup

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

-- data GameOutCome = DrawGame | Win PId | Restart

-- data PassSignal = NextPhase | Pass | Resolve

type Game a = State GameState a

