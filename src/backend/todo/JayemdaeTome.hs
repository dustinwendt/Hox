module JayemdaeTome where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


jayemdaeTome = (properties.name .~ "Jayemdae Tome") . (properties.manaCost ?~ [GenSym 4]) . (properties.color .~ []) . (properties.identity .~ []) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Artifact] []) . (properties.oracleText .~ "{4}, {T}: Draw a card.") $ defaultCard
