module Swamp where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


swamp = (properties.name .~ "Swamp") . (properties.color .~ []) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [Basic] [Land] [LType Swamp]) . (properties.oracleText .~ "({T}: Add {B}.)") $ defaultCard
