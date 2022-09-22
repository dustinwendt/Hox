module BlueElementalBlast where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


blueElementalBlast = (properties.name .~ "Blue Elemental Blast") . (properties.manaCost ?~ [CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Instant] []) . (properties.oracleText .~ "Choose one —\n• Counter target red spell.\n• Destroy target red permanent.") $ defaultCard
