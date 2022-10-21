module BirdsofParadise where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


birdsofParadise = (properties.name .~ "Birds of Paradise") . (properties.manaCost ?~ [CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ [Flying]) . (properties.typeLine .~ TypeLine [] [Creature] [CType Bird]) . (properties.oracleText .~ "Flying\n{T}: Add one mana of any color.") . (properties.power .~ (Just (PT 0))) . (properties.toughness .~ (Just (PT 1))) $ defaultCard
