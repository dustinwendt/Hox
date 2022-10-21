module StreamofLife where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


streamofLife = (properties.name .~ "Stream of Life") . (properties.manaCost ?~ [XSym,CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Sorcery] []) . (properties.oracleText .~ "Target player gains X life.") $ defaultCard
