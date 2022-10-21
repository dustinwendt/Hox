module FireElemental where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


fireElemental = (properties.name .~ "Fire Elemental") . (properties.manaCost ?~ [GenSym 3,CSym (Colored Red),CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Elemental]) . (properties.power .~ (Just (PT 5))) . (properties.toughness .~ (Just (PT 4))) $ defaultCard
