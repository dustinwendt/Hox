module Manabarbs where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


manabarbs = (properties.name .~ "Manabarbs") . (properties.manaCost ?~ [GenSym 3,CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] []) . (properties.oracleText .~ "Whenever a player taps a land for mana, Manabarbs deals 1 damage to that player.") $ defaultCard
