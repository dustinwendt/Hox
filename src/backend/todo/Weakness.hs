module Weakness where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


weakness = (properties.name .~ "Weakness") . (properties.manaCost ?~ [CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant creature\nEnchanted creature gets -2/-1.") $ defaultCard
