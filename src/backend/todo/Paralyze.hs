module Paralyze where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


paralyze = (properties.name .~ "Paralyze") . (properties.manaCost ?~ [CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant creature\nWhen Paralyze enters the battlefield, tap enchanted creature.\nEnchanted creature doesn't untap during its controller's untap step.\nAt the beginning of the upkeep of enchanted creature's controller, that player may pay {4}. If the player does, untap the creature.") $ defaultCard
