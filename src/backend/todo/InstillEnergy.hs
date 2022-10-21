module InstillEnergy where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


instillEnergy = (properties.name .~ "Instill Energy") . (properties.manaCost ?~ [CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant creature\nEnchanted creature can attack as though it had haste.\n{0}: Untap enchanted creature. Activate only during your turn and only once each turn.") $ defaultCard
