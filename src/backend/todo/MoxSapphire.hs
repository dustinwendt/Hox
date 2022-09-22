module MoxSapphire where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


moxSapphire = (properties.name .~ "Mox Sapphire") . (properties.manaCost ?~ [GenSym 0]) . (properties.color .~ []) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Artifact] []) . (properties.oracleText .~ "{T}: Add {U}.") $ defaultCard
