module EarthElemental where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


earthElemental = (properties.name .~ "Earth Elemental") . (properties.manaCost ?~ [GenSym 3,CSym (Colored Red),CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Elemental]) . (properties.power .~ (Just (PT 4))) . (properties.toughness .~ (Just (PT 5))) $ defaultCard
