module Disintegrate where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


disintegrate = (properties.name .~ "Disintegrate") . (properties.manaCost ?~ [XSym,CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Sorcery] []) . (properties.oracleText .~ "Disintegrate deals X damage to any target. If it's a creature, it can't be regenerated this turn, and if it would die this turn, exile it instead.") $ defaultCard
