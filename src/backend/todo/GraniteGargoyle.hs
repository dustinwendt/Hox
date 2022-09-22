module GraniteGargoyle where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


graniteGargoyle = (properties.name .~ "Granite Gargoyle") . (properties.manaCost ?~ [GenSym 2,CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ [Flying]) . (properties.typeLine .~ TypeLine [] [Creature] [CType Gargoyle]) . (properties.oracleText .~ "Flying\n{R}: Granite Gargoyle gets +0/+1 until end of turn.") . (properties.power .~ (Just (PT 2))) . (properties.toughness .~ (Just (PT 2))) $ defaultCard
