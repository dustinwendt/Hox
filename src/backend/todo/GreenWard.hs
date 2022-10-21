module GreenWard where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


greenWard = (properties.name .~ "Green Ward") . (properties.manaCost ?~ [CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant creature\nEnchanted creature has protection from green. This effect doesn't remove Green Ward.") $ defaultCard
