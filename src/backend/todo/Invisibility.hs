module Invisibility where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


invisibility = (properties.name .~ "Invisibility") . (properties.manaCost ?~ [CSym (Colored Blue),CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant creature\nEnchanted creature can't be blocked except by Walls.") $ defaultCard
