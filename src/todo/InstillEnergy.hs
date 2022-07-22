module InstillEnergy where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


instillEnergy = (properties.name .~ "Instill Energy") . (properties.manaCost ?~ [CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant creature
Enchanted creature can attack as though it had haste.
{0}: Untap enchanted creature. Activate only during your turn and only once each turn.") $ defaultCard
