module AnimateWall where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


animateWall = (properties.name .~ "Animate Wall") . (properties.manaCost ?~ [CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant Wall\nEnchanted Wall can attack as though it didn't have defender.") $ defaultCard
