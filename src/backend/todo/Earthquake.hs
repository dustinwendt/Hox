module Earthquake where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


earthquake = (properties.name .~ "Earthquake") . (properties.manaCost ?~ [XSym,CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Sorcery] []) . (properties.oracleText .~ "Earthquake deals X damage to each creature without flying and each player.") $ defaultCard
