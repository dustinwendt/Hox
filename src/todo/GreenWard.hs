module GreenWard where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


greenWard = (properties.name .~ "Green Ward") . (properties.manaCost ?~ [CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant creature
Enchanted creature has protection from green. This effect doesn't remove Green Ward.") $ defaultCard
