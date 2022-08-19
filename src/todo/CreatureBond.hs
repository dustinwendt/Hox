module CreatureBond where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


creatureBond = (properties.name .~ "Creature Bond") . (properties.manaCost ?~ [GenSym 1,CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant creature\nWhen enchanted creature dies, Creature Bond deals damage equal to that creature's toughness to the creature's controller.") $ defaultCard
