module Disenchant where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


disenchant = (properties.name .~ "Disenchant") . (properties.manaCost ?~ [GenSym 1,CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Instant] []) . (properties.oracleText .~ "Destroy target artifact or enchantment.") $ defaultCard
