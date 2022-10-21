module WhiteKnight where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


whiteKnight = (properties.name .~ "White Knight") . (properties.manaCost ?~ [CSym (Colored White),CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ [Protection]) . (properties.typeLine .~ TypeLine [] [Creature] [CType Human,CType Knight]) . (properties.oracleText .~ "First strike (This creature deals combat damage before creatures without first strike.)\nProtection from black (This creature can't be blocked, targeted, dealt damage, or enchanted by anything black.)") . (properties.power .~ (Just (PT 2))) . (properties.toughness .~ (Just (PT 2))) $ defaultCard
