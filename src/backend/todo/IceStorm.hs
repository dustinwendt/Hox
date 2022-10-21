module IceStorm where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


iceStorm = (properties.name .~ "Ice Storm") . (properties.manaCost ?~ [GenSym 2,CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Sorcery] []) . (properties.oracleText .~ "Destroy target land.") $ defaultCard
