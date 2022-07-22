module LordofAtlantis where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


lordofAtlantis = (properties.name .~ "Lord of Atlantis") . (properties.manaCost ?~ [CSym (Colored Blue),CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Merfolk]) . (properties.oracleText .~ "Other Merfolk get +1/+1 and have islandwalk. (They can't be blocked as long as defending player controls an Island.)") . (properties.power .~ (Just (PT 2))) . (properties.toughness .~ (Just (PT 2))) $ defaultCard
