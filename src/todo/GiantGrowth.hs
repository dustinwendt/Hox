module GiantGrowth where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


giantGrowth = (properties.name .~ "Giant Growth") . (properties.manaCost ?~ [CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Instant] []) . (properties.oracleText .~ "Target creature gets +3/+3 until end of turn.") $ defaultCard
