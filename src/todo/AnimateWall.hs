module AnimateWall where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


animateWall = (properties.name .~ "Animate Wall") . (properties.manaCost ?~ [CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant Wall
Enchanted Wall can attack as though it didn't have defender.") $ defaultCard
