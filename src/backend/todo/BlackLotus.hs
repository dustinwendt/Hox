module BlackLotus where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


blackLotus = (properties.name .~ "Black Lotus") . (properties.manaCost ?~ [GenSym 0]) . (properties.color .~ []) . (properties.identity .~ []) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Artifact] []) . (properties.oracleText .~ "{T}, Sacrifice Black Lotus: Add three mana of any one color.") $ defaultCard
