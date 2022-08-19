module Conversion where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


conversion = (properties.name .~ "Conversion") . (properties.manaCost ?~ [GenSym 2,CSym (Colored White),CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] []) . (properties.oracleText .~ "At the beginning of your upkeep, sacrifice Conversion unless you pay {W}{W}.\nAll Mountains are Plains.") $ defaultCard
