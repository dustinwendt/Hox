module Taiga where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


taiga = (properties.name .~ "Taiga") . (properties.color .~ []) . (properties.identity .~ [Green,Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Land] [LType Mountain,LType Forest]) . (properties.oracleText .~ "({T}: Add {R} or {G}.)") $ defaultCard
