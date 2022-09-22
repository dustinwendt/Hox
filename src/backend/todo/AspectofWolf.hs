module AspectofWolf where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


aspectofWolf = (properties.name .~ "Aspect of Wolf") . (properties.manaCost ?~ [GenSym 1,CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant creature\nEnchanted creature gets +X/+Y, where X is half the number of Forests you control, rounded down, and Y is half the number of Forests you control, rounded up.") $ defaultCard
