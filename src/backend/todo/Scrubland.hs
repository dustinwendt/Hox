module Scrubland where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


scrubland = (properties.name .~ "Scrubland") . (properties.color .~ []) . (properties.identity .~ [Black,White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Land] [LType Plains,LType Swamp]) . (properties.oracleText .~ "({T}: Add {W} or {B}.)") $ defaultCard
