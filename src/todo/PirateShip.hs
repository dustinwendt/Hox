module PirateShip where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


pirateShip = (properties.name .~ "Pirate Ship") . (properties.manaCost ?~ [GenSym 4,CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Human,CType Pirate]) . (properties.oracleText .~ "Pirate Ship can't attack unless defending player controls an Island.
{T}: Pirate Ship deals 1 damage to any target.
When you control no Islands, sacrifice Pirate Ship.") . (properties.power .~ (Just (PT 4))) . (properties.toughness .~ (Just (PT 3))) $ defaultCard
