module ControlMagic where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


controlMagic = (properties.name .~ "Control Magic") . (properties.manaCost ?~ [GenSym 2,CSym (Colored Blue),CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant creature\nYou control enchanted creature.") $ defaultCard
