module StoneRain where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


stoneRain = (properties.name .~ "Stone Rain") . (properties.manaCost ?~ [GenSym 2,CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Sorcery] []) . (properties.oracleText .~ "Destroy target land.") $ defaultCard
