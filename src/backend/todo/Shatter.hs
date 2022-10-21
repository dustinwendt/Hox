module Shatter where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


shatter = (properties.name .~ "Shatter") . (properties.manaCost ?~ [GenSym 1,CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Instant] []) . (properties.oracleText .~ "Destroy target artifact.") $ defaultCard
