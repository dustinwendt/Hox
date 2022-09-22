module BogWraith where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


bogWraith = (properties.name .~ "Bog Wraith") . (properties.manaCost ?~ [GenSym 3,CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Wraith]) . (properties.oracleText .~ "Swampwalk (This creature can't be blocked as long as defending player controls a Swamp.)") . (properties.power .~ (Just (PT 3))) . (properties.toughness .~ (Just (PT 3))) $ defaultCard
