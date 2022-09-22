module FrozenShade where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


frozenShade = (properties.name .~ "Frozen Shade") . (properties.manaCost ?~ [GenSym 2,CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Shade]) . (properties.oracleText .~ "{B}: Frozen Shade gets +1/+1 until end of turn.") . (properties.power .~ (Just (PT 0))) . (properties.toughness .~ (Just (PT 1))) $ defaultCard
