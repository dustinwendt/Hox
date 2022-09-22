module Purelace where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


purelace = (properties.name .~ "Purelace") . (properties.manaCost ?~ [CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Instant] []) . (properties.oracleText .~ "Target spell or permanent becomes white. (Mana symbols on that permanent remain unchanged.)") $ defaultCard
