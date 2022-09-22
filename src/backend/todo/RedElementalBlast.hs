module RedElementalBlast where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


redElementalBlast = (properties.name .~ "Red Elemental Blast") . (properties.manaCost ?~ [CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Instant] []) . (properties.oracleText .~ "Choose one —\n• Counter target blue spell.\n• Destroy target blue permanent.") $ defaultCard
