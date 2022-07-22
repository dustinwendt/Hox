module Jump where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


jump = (properties.name .~ "Jump") . (properties.manaCost ?~ [CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Instant] []) . (properties.oracleText .~ "Target creature gains flying until end of turn.") $ defaultCard
