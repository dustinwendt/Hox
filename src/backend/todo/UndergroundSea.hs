module UndergroundSea where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


undergroundSea = (properties.name .~ "Underground Sea") . (properties.color .~ []) . (properties.identity .~ [Black,Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Land] [LType Island,LType Swamp]) . (properties.oracleText .~ "({T}: Add {U} or {B}.)") $ defaultCard
