module BlueWard where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


blueWard = (properties.name .~ "Blue Ward") . (properties.manaCost ?~ [CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant creature
Enchanted creature has protection from blue. This effect doesn't remove Blue Ward.") $ defaultCard
