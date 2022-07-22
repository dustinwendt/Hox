module LordofthePit where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


lordofthePit = (properties.name .~ "Lord of the Pit") . (properties.manaCost ?~ [GenSym 4,CSym (Colored Black),CSym (Colored Black),CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ [Flying,Trample]) . (properties.typeLine .~ TypeLine [] [Creature] [CType Demon]) . (properties.oracleText .~ "Flying, trample
At the beginning of your upkeep, sacrifice a creature other than Lord of the Pit. If you can't, Lord of the Pit deals 7 damage to you.") . (properties.power .~ (Just (PT 7))) . (properties.toughness .~ (Just (PT 7))) $ defaultCard
