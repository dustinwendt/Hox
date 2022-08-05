module PowerLeak where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


powerLeak = (properties.name .~ "Power Leak") . (properties.manaCost ?~ [GenSym 1,CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant enchantment
At the beginning of the upkeep of enchanted enchantment's controller, that player may pay any amount of mana. Power Leak deals 2 damage to that player. Prevent X of that damage, where X is the amount of mana that player paid this way.") $ defaultCard
