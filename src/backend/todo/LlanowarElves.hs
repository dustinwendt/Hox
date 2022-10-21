module LlanowarElves where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


llanowarElves = (properties.name .~ "Llanowar Elves") . (properties.manaCost ?~ [CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Elf,CType Druid]) . (properties.oracleText .~ "{T}: Add {G}.") . (properties.power .~ (Just (PT 1))) . (properties.toughness .~ (Just (PT 1))) $ defaultCard
