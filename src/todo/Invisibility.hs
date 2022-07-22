module Invisibility where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


invisibility = (properties.name .~ "Invisibility") . (properties.manaCost ?~ [CSym (Colored Blue),CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant creature
Enchanted creature can't be blocked except by Walls.") $ defaultCard
