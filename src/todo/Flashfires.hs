module Flashfires where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


flashfires = (properties.name .~ "Flashfires") . (properties.manaCost ?~ [GenSym 3,CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Sorcery] []) . (properties.oracleText .~ "Destroy all Plains.") $ defaultCard
