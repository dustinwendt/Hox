module Tundra where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


tundra = (properties.name .~ "Tundra") . (properties.color .~ []) . (properties.identity .~ [Blue,White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Land] [LType Plains,LType Island]) . (properties.oracleText .~ "({T}: Add {W} or {U}.)") $ defaultCard
