module WaterElemental where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


waterElemental = (properties.name .~ "Water Elemental") . (properties.manaCost ?~ [GenSym 3,CSym (Colored Blue),CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Elemental]) . (properties.power .~ (Just (PT 5))) . (properties.toughness .~ (Just (PT 4))) $ defaultCard
