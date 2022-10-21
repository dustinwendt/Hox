module RocofKherRidges where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


rocofKherRidges = (properties.name .~ "Roc of Kher Ridges") . (properties.manaCost ?~ [GenSym 3,CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ [Flying]) . (properties.typeLine .~ TypeLine [] [Creature] [CType Bird]) . (properties.oracleText .~ "Flying") . (properties.power .~ (Just (PT 3))) . (properties.toughness .~ (Just (PT 3))) $ defaultCard
