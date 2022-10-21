module HolyArmor where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


holyArmor = (properties.name .~ "Holy Armor") . (properties.manaCost ?~ [CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant creature\nEnchanted creature gets +0/+2.\n{W}: Enchanted creature gets +0/+1 until end of turn.") $ defaultCard
