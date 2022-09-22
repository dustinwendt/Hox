module GoblinBalloonBrigade where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


goblinBalloonBrigade = (properties.name .~ "Goblin Balloon Brigade") . (properties.manaCost ?~ [CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Goblin,CType Warrior]) . (properties.oracleText .~ "{R}: Goblin Balloon Brigade gains flying until end of turn.") . (properties.power .~ (Just (PT 1))) . (properties.toughness .~ (Just (PT 1))) $ defaultCard
