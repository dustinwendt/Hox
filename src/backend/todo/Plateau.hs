module Plateau where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


plateau = (properties.name .~ "Plateau") . (properties.color .~ []) . (properties.identity .~ [Red,White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Land] [LType Mountain,LType Plains]) . (properties.oracleText .~ "({T}: Add {R} or {W}.)") $ defaultCard
