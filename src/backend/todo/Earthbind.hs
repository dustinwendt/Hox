module Earthbind where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


earthbind = (properties.name .~ "Earthbind") . (properties.manaCost ?~ [CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant creature\nWhen Earthbind enters the battlefield, if enchanted creature has flying, Earthbind deals 2 damage to that creature and Earthbind gains \"Enchanted creature loses flying.\"") $ defaultCard
