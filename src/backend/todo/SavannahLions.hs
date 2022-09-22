module SavannahLions where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


savannahLions = (properties.name .~ "Savannah Lions") . (properties.manaCost ?~ [CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Cat]) . (properties.power .~ (Just (PT 2))) . (properties.toughness .~ (Just (PT 1))) $ defaultCard
