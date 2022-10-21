module Bayou where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


bayou = (properties.name .~ "Bayou") . (properties.color .~ []) . (properties.identity .~ [Black,Green]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Land] [LType Swamp,LType Forest]) . (properties.oracleText .~ "({T}: Add {B} or {G}.)") $ defaultCard
