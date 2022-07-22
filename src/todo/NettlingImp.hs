module NettlingImp where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


nettlingImp = (properties.name .~ "Nettling Imp") . (properties.manaCost ?~ [GenSym 2,CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Imp]) . (properties.oracleText .~ "{T}: Choose target non-Wall creature the active player has controlled continuously since the beginning of the turn. That creature attacks this turn if able. Destroy it at the beginning of the next end step if it didn't attack this turn. Activate only during an opponent's turn, before attackers are declared.") . (properties.power .~ (Just (PT 1))) . (properties.toughness .~ (Just (PT 1))) $ defaultCard
