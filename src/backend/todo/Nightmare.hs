module Nightmare where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


nightmare = (properties.name .~ "Nightmare") . (properties.manaCost ?~ [GenSym 5,CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ [Flying]) . (properties.typeLine .~ TypeLine [] [Creature] [CType Nightmare,CType Horse]) . (properties.oracleText .~ "Flying (This creature can't be blocked except by creatures with flying or reach.)\nNightmare's power and toughness are each equal to the number of Swamps you control.") . (properties.power .~ (Just (Star))) . (properties.toughness .~ (Just (Star))) $ defaultCard
