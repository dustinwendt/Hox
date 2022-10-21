module Forest where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


forest = (properties.name .~ "Forest") . (properties.color .~ []) . (properties.identity .~ [Green]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [Basic] [Land] [LType Forest]) . (properties.oracleText .~ "({T}: Add {G}.)") $ defaultCard
