module Fork where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


fork = (properties.name .~ "Fork") . (properties.manaCost ?~ [CSym (Colored Red),CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Instant] []) . (properties.oracleText .~ "Copy target instant or sorcery spell, except that the copy is red. You may choose new targets for the copy.") $ defaultCard
