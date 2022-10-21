module CircleofProtectionGreen where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


circleofProtectionGreen = (properties.name .~ "Circle of Protection: Green") . (properties.manaCost ?~ [GenSym 1,CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] []) . (properties.oracleText .~ "{1}: The next time a green source of your choice would deal damage to you this turn, prevent that damage.") $ defaultCard
