module Paralyze where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


paralyze = (properties.name .~ "Paralyze") . (properties.manaCost ?~ [CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant creature
When Paralyze enters the battlefield, tap enchanted creature.
Enchanted creature doesn't untap during its controller's untap step.
At the beginning of the upkeep of enchanted creature's controller, that player may pay {4}. If the player does, untap the creature.") $ defaultCard
