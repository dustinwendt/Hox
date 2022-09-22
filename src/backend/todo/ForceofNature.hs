module ForceofNature where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


forceofNature = (properties.name .~ "Force of Nature") . (properties.manaCost ?~ [GenSym 2,CSym (Colored Green),CSym (Colored Green),CSym (Colored Green),CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ [Trample]) . (properties.typeLine .~ TypeLine [] [Creature] [CType Elemental]) . (properties.oracleText .~ "Trample (This creature can deal excess combat damage to the player or planeswalker it's attacking.)\nAt the beginning of your upkeep, Force of Nature deals 8 damage to you unless you pay {G}{G}{G}{G}.") . (properties.power .~ (Just (PT 8))) . (properties.toughness .~ (Just (PT 8))) $ defaultCard
