module Wanderlust where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


wanderlust = (properties.name .~ "Wanderlust") . (properties.manaCost ?~ [GenSym 2,CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant creature\nAt the beginning of the upkeep of enchanted creature's controller, Wanderlust deals 1 damage to that player.") $ defaultCard
