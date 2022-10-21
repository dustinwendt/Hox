module TropicalIsland where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


tropicalIsland = (properties.name .~ "Tropical Island") . (properties.color .~ []) . (properties.identity .~ [Green,Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Land] [LType Forest,LType Island]) . (properties.oracleText .~ "({T}: Add {G} or {U}.)") $ defaultCard
