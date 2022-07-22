module Island where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


island = (properties.name .~ "Island") . (properties.color .~ []) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [Basic] [Land] [LType Island]) . (properties.oracleText .~ "({T}: Add {U}.)") $ defaultCard
