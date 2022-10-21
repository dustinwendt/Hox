module Simulacrum where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


simulacrum = (properties.name .~ "Simulacrum") . (properties.manaCost ?~ [GenSym 1,CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Instant] []) . (properties.oracleText .~ "You gain life equal to the damage dealt to you this turn. Simulacrum deals damage to target creature you control equal to the damage dealt to you this turn.") $ defaultCard
