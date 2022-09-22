module Righteousness where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


righteousness = (properties.name .~ "Righteousness") . (properties.manaCost ?~ [CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Instant] []) . (properties.oracleText .~ "Target blocking creature gets +7/+7 until end of turn.") $ defaultCard
