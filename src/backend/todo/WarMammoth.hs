module WarMammoth where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


warMammoth = (properties.name .~ "War Mammoth") . (properties.manaCost ?~ [GenSym 3,CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ [Trample]) . (properties.typeLine .~ TypeLine [] [Creature] [CType Elephant]) . (properties.oracleText .~ "Trample") . (properties.power .~ (Just (PT 3))) . (properties.toughness .~ (Just (PT 3))) $ defaultCard
