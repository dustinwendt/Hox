module MoxPearl where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


moxPearl = (properties.name .~ "Mox Pearl") . (properties.manaCost ?~ [GenSym 0]) . (properties.color .~ []) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Artifact] []) . (properties.oracleText .~ "{T}: Add {W}.") $ defaultCard
