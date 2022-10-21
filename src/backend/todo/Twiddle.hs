module Twiddle where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


twiddle = (properties.name .~ "Twiddle") . (properties.manaCost ?~ [CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Instant] []) . (properties.oracleText .~ "You may tap or untap target artifact, creature, or land.") $ defaultCard
