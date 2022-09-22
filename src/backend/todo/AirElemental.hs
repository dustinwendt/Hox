module AirElemental where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


airElemental = (properties.name .~ "Air Elemental") . (properties.manaCost ?~ [GenSym 3,CSym (Colored Blue),CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ [Flying]) . (properties.typeLine .~ TypeLine [] [Creature] [CType Elemental]) . (properties.oracleText .~ "Flying") . (properties.power .~ (Just (PT 4))) . (properties.toughness .~ (Just (PT 4))) $ defaultCard
