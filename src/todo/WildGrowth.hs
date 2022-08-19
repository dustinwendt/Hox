module WildGrowth where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


wildGrowth = (properties.name .~ "Wild Growth") . (properties.manaCost ?~ [CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant land\nWhenever enchanted land is tapped for mana, its controller adds an additional {G}.") $ defaultCard
