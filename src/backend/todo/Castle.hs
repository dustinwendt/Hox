module Castle where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


castle = (properties.name .~ "Castle") . (properties.manaCost ?~ [GenSym 3,CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] []) . (properties.oracleText .~ "Untapped creatures you control get +0/+2.") $ defaultCard
