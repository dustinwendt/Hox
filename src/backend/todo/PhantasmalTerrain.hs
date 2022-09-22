module PhantasmalTerrain where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


phantasmalTerrain = (properties.name .~ "Phantasmal Terrain") . (properties.manaCost ?~ [CSym (Colored Blue),CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant land\nAs Phantasmal Terrain enters the battlefield, choose a basic land type.\nEnchanted land is the chosen type.") $ defaultCard
