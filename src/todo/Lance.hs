module Lance where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


lance = (properties.name .~ "Lance") . (properties.manaCost ?~ [CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant creature
Enchanted creature has first strike.") $ defaultCard
