module Burrowing where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


burrowing = (properties.name .~ "Burrowing") . (properties.manaCost ?~ [CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant creature
Enchanted creature has mountainwalk. (It can't be blocked as long as defending player controls a Mountain.)") $ defaultCard
