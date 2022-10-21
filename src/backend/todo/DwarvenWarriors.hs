module DwarvenWarriors where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


dwarvenWarriors = (properties.name .~ "Dwarven Warriors") . (properties.manaCost ?~ [GenSym 2,CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Dwarf,CType Warrior]) . (properties.oracleText .~ "{T}: Target creature with power 2 or less can't be blocked this turn.") . (properties.power .~ (Just (PT 1))) . (properties.toughness .~ (Just (PT 1))) $ defaultCard
