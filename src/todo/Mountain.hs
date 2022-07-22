module Mountain where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


mountain = (properties.name .~ "Mountain") . (properties.color .~ []) . (properties.identity .~ [Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [Basic] [Land] [LType Mountain]) . (properties.oracleText .~ "({T}: Add {R}.)") $ defaultCard
