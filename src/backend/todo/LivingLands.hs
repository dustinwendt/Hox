module LivingLands where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


livingLands = (properties.name .~ "Living Lands") . (properties.manaCost ?~ [GenSym 3,CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] []) . (properties.oracleText .~ "All Forests are 1/1 creatures that are still lands.") $ defaultCard
