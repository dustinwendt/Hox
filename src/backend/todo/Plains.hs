module Plains where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


plains = (properties.name .~ "Plains") . (properties.color .~ []) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [Basic] [Land] [LType Plains]) . (properties.oracleText .~ "({T}: Add {W}.)") $ defaultCard
