module MerfolkofthePearlTrident where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


merfolkofthePearlTrident = (properties.name .~ "Merfolk of the Pearl Trident") . (properties.manaCost ?~ [CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Merfolk]) . (properties.power .~ (Just (PT 1))) . (properties.toughness .~ (Just (PT 1))) $ defaultCard
