module Feedback where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


feedback = (properties.name .~ "Feedback") . (properties.manaCost ?~ [GenSym 2,CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant enchantment
At the beginning of the upkeep of enchanted enchantment's controller, Feedback deals 1 damage to that player.") $ defaultCard
