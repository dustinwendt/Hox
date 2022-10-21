module BadMoon where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


badMoon = (properties.name .~ "Bad Moon") . (properties.manaCost ?~ [GenSym 1,CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] []) . (properties.oracleText .~ "Black creatures get +1/+1.") $ defaultCard
