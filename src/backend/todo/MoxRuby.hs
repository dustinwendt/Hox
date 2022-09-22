module MoxRuby where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


moxRuby = (properties.name .~ "Mox Ruby") . (properties.manaCost ?~ [GenSym 0]) . (properties.color .~ []) . (properties.identity .~ [Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Artifact] []) . (properties.oracleText .~ "{T}: Add {R}.") $ defaultCard
