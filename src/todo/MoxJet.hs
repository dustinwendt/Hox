module MoxJet where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


moxJet = (properties.name .~ "Mox Jet") . (properties.manaCost ?~ [GenSym 0]) . (properties.color .~ []) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Artifact] []) . (properties.oracleText .~ "{T}: Add {B}.") $ defaultCard
