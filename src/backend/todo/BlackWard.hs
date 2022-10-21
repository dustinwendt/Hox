module BlackWard where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


blackWard = (properties.name .~ "Black Ward") . (properties.manaCost ?~ [CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant creature\nEnchanted creature has protection from black. This effect doesn't remove Black Ward.") $ defaultCard
