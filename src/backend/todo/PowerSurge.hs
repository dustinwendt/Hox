module PowerSurge where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


powerSurge = (properties.name .~ "Power Surge") . (properties.manaCost ?~ [CSym (Colored Red),CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] []) . (properties.oracleText .~ "At the beginning of each player's upkeep, Power Surge deals X damage to that player, where X is the number of untapped lands they controlled at the beginning of this turn.") $ defaultCard
