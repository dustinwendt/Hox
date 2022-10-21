module TimeWalk where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


timeWalk = (properties.name .~ "Time Walk") . (properties.manaCost ?~ [GenSym 1,CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Sorcery] []) . (properties.oracleText .~ "Take an extra turn after this one.") $ defaultCard
