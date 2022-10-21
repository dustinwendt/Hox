module DrainLife where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


drainLife = (properties.name .~ "Drain Life") . (properties.manaCost ?~ [XSym,GenSym 1,CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Sorcery] []) . (properties.oracleText .~ "Spend only black mana on X.\nDrain Life deals X damage to any target. You gain life equal to the damage dealt, but not more life than the player's life total before the damage was dealt, the planeswalker's loyalty before the damage was dealt, or the creature's toughness.") $ defaultCard
