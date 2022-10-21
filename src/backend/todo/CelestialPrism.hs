module CelestialPrism where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


celestialPrism = (properties.name .~ "Celestial Prism") . (properties.manaCost ?~ [GenSym 3]) . (properties.color .~ []) . (properties.identity .~ []) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Artifact] []) . (properties.oracleText .~ "{2}, {T}: Add one mana of any color.") $ defaultCard
