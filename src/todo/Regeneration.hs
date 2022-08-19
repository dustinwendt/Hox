module Regeneration where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


regeneration = (properties.name .~ "Regeneration") . (properties.manaCost ?~ [GenSym 1,CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant creature (Target a creature as you cast this. This card enters the battlefield attached to that creature.)\n{G}: Regenerate enchanted creature. (The next time that creature would be destroyed this turn, it isn't. Instead tap it, remove all damage from it, and remove it from combat.)") $ defaultCard
