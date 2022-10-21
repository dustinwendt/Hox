module Blessing where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


blessing = (properties.name .~ "Blessing") . (properties.manaCost ?~ [CSym (Colored White),CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant creature\n{W}: Enchanted creature gets +1/+1 until end of turn.") $ defaultCard
