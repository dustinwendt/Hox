module SengirVampire where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


sengirVampire = (properties.name .~ "Sengir Vampire") . (properties.manaCost ?~ [GenSym 3,CSym (Colored Black),CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ [Flying]) . (properties.typeLine .~ TypeLine [] [Creature] [CType Vampire]) . (properties.oracleText .~ "Flying (This creature can't be blocked except by creatures with flying or reach.)
Whenever a creature dealt damage by Sengir Vampire this turn dies, put a +1/+1 counter on Sengir Vampire.") . (properties.power .~ (Just (PT 4))) . (properties.toughness .~ (Just (PT 4))) $ defaultCard
