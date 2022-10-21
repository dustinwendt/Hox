module WheelofFortune where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


wheelofFortune = (properties.name .~ "Wheel of Fortune") . (properties.manaCost ?~ [GenSym 2,CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Sorcery] []) . (properties.oracleText .~ "Each player discards their hand, then draws seven cards.") $ defaultCard
