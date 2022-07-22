module CursedLand where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


cursedLand = (properties.name .~ "Cursed Land") . (properties.manaCost ?~ [GenSym 2,CSym (Colored Black),CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant land
At the beginning of the upkeep of enchanted land's controller, Cursed Land deals 1 damage to that player.") $ defaultCard
