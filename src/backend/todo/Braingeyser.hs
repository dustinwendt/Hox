module Braingeyser where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


braingeyser = (properties.name .~ "Braingeyser") . (properties.manaCost ?~ [XSym,CSym (Colored Blue),CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Sorcery] []) . (properties.oracleText .~ "Target player draws X cards.") $ defaultCard
