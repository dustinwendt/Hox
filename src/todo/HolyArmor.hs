module HolyArmor where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


holyArmor = (properties.name .~ "Holy Armor") . (properties.manaCost ?~ [CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant creature
Enchanted creature gets +0/+2.
{W}: Enchanted creature gets +0/+1 until end of turn.") $ defaultCard
