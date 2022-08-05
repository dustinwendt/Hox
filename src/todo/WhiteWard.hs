module WhiteWard where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


whiteWard = (properties.name .~ "White Ward") . (properties.manaCost ?~ [CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant creature
Enchanted creature has protection from white. This effect doesn't remove White Ward.") $ defaultCard
