module ScrybSprites where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


scrybSprites = (properties.name .~ "Scryb Sprites") . (properties.manaCost ?~ [CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ [Flying]) . (properties.typeLine .~ TypeLine [] [Creature] [CType Faerie]) . (properties.oracleText .~ "Flying") . (properties.power .~ (Just (PT 1))) . (properties.toughness .~ (Just (PT 1))) $ defaultCard
