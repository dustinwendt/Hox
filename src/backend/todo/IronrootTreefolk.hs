module IronrootTreefolk where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


ironrootTreefolk = (properties.name .~ "Ironroot Treefolk") . (properties.manaCost ?~ [GenSym 4,CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Treefolk]) . (properties.power .~ (Just (PT 3))) . (properties.toughness .~ (Just (PT 5))) $ defaultCard
