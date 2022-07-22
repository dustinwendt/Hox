module Fireball where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


fireball = (properties.name .~ "Fireball") . (properties.manaCost ?~ [XSym,CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Sorcery] []) . (properties.oracleText .~ "This spell costs {1} more to cast for each target beyond the first.
Fireball deals X damage divided evenly, rounded down, among any number of targets.") $ defaultCard
