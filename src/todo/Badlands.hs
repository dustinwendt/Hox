module Badlands where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


badlands = (properties.name .~ "Badlands") . (properties.color .~ []) . (properties.identity .~ [Black,Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Land] [LType Swamp,LType Mountain]) . (properties.oracleText .~ "({T}: Add {B} or {R}.)") $ defaultCard
