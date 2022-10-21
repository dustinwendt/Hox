module Tsunami where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


tsunami = (properties.name .~ "Tsunami") . (properties.manaCost ?~ [GenSym 3,CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Sorcery] []) . (properties.oracleText .~ "Destroy all Islands.") $ defaultCard
