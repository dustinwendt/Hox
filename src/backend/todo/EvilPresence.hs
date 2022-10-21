module EvilPresence where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


evilPresence = (properties.name .~ "Evil Presence") . (properties.manaCost ?~ [CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant land\nEnchanted land is a Swamp.") $ defaultCard
