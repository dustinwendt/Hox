module SolRing where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


solRing = (properties.name .~ "Sol Ring") . (properties.manaCost ?~ [GenSym 1]) . (properties.color .~ []) . (properties.identity .~ []) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Artifact] []) . (properties.oracleText .~ "{T}: Add {C}{C}.") $ defaultCard
