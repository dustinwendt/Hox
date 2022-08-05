module Weakness where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


weakness = (properties.name .~ "Weakness") . (properties.manaCost ?~ [CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant creature
Enchanted creature gets -2/-1.") $ defaultCard
