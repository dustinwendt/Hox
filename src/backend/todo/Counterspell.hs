module Counterspell where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


counterspell = (properties.name .~ "Counterspell") . (properties.manaCost ?~ [CSym (Colored Blue),CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Instant] []) . (properties.oracleText .~ "Counter target spell.") $ defaultCard
