module Flight where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


flight = (properties.name .~ "Flight") . (properties.manaCost ?~ [CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant creature
Enchanted creature has flying.") $ defaultCard
