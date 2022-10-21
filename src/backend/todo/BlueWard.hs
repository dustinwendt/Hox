module BlueWard where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


blueWard = (properties.name .~ "Blue Ward") . (properties.manaCost ?~ [CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant creature\nEnchanted creature has protection from blue. This effect doesn't remove Blue Ward.") $ defaultCard
