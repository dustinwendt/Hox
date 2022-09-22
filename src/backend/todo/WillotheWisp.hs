module WillotheWisp where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


willotheWisp = (properties.name .~ "Will-o'-the-Wisp") . (properties.manaCost ?~ [CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ [Flying]) . (properties.typeLine .~ TypeLine [] [Creature] [CType Spirit]) . (properties.oracleText .~ "Flying (This creature can't be blocked except by creatures with flying or reach.)\n{B}: Regenerate Will-o'-the-Wisp. (The next time this creature would be destroyed this turn, it isn't. Instead tap it, remove all damage from it, and remove it from combat.)") . (properties.power .~ (Just (PT 0))) . (properties.toughness .~ (Just (PT 1))) $ defaultCard
