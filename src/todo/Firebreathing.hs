module Firebreathing where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


firebreathing = (properties.name .~ "Firebreathing") . (properties.manaCost ?~ [CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant creature
{R}: Enchanted creature gets +1/+0 until end of turn.") $ defaultCard
