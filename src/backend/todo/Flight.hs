module Flight where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


flight = (properties.name .~ "Flight") . (properties.manaCost ?~ [CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant creature\nEnchanted creature has flying.") $ defaultCard
