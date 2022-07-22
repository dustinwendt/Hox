module CircleofProtectionWhite where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


circleofProtectionWhite = (properties.name .~ "Circle of Protection: White") . (properties.manaCost ?~ [GenSym 1,CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] []) . (properties.oracleText .~ "{1}: The next time a white source of your choice would deal damage to you this turn, prevent that damage.") $ defaultCard
