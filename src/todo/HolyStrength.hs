module HolyStrength where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


holyStrength = (properties.name .~ "Holy Strength") . (properties.manaCost ?~ [CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant creature
Enchanted creature gets +1/+2.") $ defaultCard
