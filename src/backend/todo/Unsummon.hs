module Unsummon where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


unsummon = (properties.name .~ "Unsummon") . (properties.manaCost ?~ [CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Instant] []) . (properties.oracleText .~ "Return target creature to its owner's hand.") $ defaultCard
