module Savannah where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


savannah = (properties.name .~ "Savannah") . (properties.color .~ []) . (properties.identity .~ [Green,White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Land] [LType Forest,LType Plains]) . (properties.oracleText .~ "({T}: Add {G} or {W}.)") $ defaultCard
