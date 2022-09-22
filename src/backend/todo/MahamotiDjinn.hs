module MahamotiDjinn where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


mahamotiDjinn = (properties.name .~ "Mahamoti Djinn") . (properties.manaCost ?~ [GenSym 4,CSym (Colored Blue),CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ [Flying]) . (properties.typeLine .~ TypeLine [] [Creature] [CType Djinn]) . (properties.oracleText .~ "Flying (This creature can't be blocked except by creatures with flying or reach.)") . (properties.power .~ (Just (PT 5))) . (properties.toughness .~ (Just (PT 6))) $ defaultCard
