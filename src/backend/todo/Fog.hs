module Fog where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


fog = (properties.name .~ "Fog") . (properties.manaCost ?~ [CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Instant] []) . (properties.oracleText .~ "Prevent all combat damage that would be dealt this turn.") $ defaultCard
