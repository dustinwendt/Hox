module HillGiant where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


hillGiant = (properties.name .~ "Hill Giant") . (properties.manaCost ?~ [GenSym 3,CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Giant]) . (properties.power .~ (Just (PT 3))) . (properties.toughness .~ (Just (PT 3))) $ defaultCard
