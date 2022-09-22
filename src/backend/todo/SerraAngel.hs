module SerraAngel where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


serraAngel = (properties.name .~ "Serra Angel") . (properties.manaCost ?~ [GenSym 3,CSym (Colored White),CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ [Flying,Vigilance]) . (properties.typeLine .~ TypeLine [] [Creature] [CType Angel]) . (properties.oracleText .~ "Flying, vigilance") . (properties.power .~ (Just (PT 4))) . (properties.toughness .~ (Just (PT 4))) $ defaultCard
