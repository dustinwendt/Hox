module Karma where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


karma = (properties.name .~ "Karma") . (properties.manaCost ?~ [GenSym 2,CSym (Colored White),CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] []) . (properties.oracleText .~ "At the beginning of each player's upkeep, Karma deals damage to that player equal to the number of Swamps they control.") $ defaultCard
