module BlackWard where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


blackWard = (properties.name .~ "Black Ward") . (properties.manaCost ?~ [CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant creature
Enchanted creature has protection from black. This effect doesn't remove Black Ward.") $ defaultCard
