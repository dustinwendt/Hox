module ElvishArchers where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


elvishArchers = (properties.name .~ "Elvish Archers") . (properties.manaCost ?~ [GenSym 1,CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Elf,CType Archer]) . (properties.oracleText .~ "First strike") . (properties.power .~ (Just (PT 2))) . (properties.toughness .~ (Just (PT 1))) $ defaultCard
