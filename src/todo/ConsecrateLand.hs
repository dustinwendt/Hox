module ConsecrateLand where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


consecrateLand = (properties.name .~ "Consecrate Land") . (properties.manaCost ?~ [CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant land\nEnchanted land has indestructible and can't be enchanted by other Auras.") $ defaultCard
