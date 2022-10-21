module DarkRitual where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


darkRitual = (properties.name .~ "Dark Ritual") . (properties.manaCost ?~ [CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Instant] []) . (properties.oracleText .~ "Add {B}{B}{B}.") $ defaultCard
