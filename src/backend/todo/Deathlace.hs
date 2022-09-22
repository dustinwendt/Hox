module Deathlace where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


deathlace = (properties.name .~ "Deathlace") . (properties.manaCost ?~ [CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Instant] []) . (properties.oracleText .~ "Target spell or permanent becomes black. (Mana symbols on that permanent remain unchanged.)") $ defaultCard
