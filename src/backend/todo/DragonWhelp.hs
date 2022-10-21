module DragonWhelp where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


dragonWhelp = (properties.name .~ "Dragon Whelp") . (properties.manaCost ?~ [GenSym 2,CSym (Colored Red),CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ [Flying]) . (properties.typeLine .~ TypeLine [] [Creature] [CType Dragon]) . (properties.oracleText .~ "Flying\n{R}: Dragon Whelp gets +1/+0 until end of turn. If this ability has been activated four or more times this turn, sacrifice Dragon Whelp at the beginning of the next end step.") . (properties.power .~ (Just (PT 2))) . (properties.toughness .~ (Just (PT 3))) $ defaultCard
