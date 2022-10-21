module GrayOgre where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


grayOgre = (properties.name .~ "Gray Ogre") . (properties.manaCost ?~ [GenSym 2,CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Ogre]) . (properties.power .~ (Just (PT 2))) . (properties.toughness .~ (Just (PT 2))) $ defaultCard
