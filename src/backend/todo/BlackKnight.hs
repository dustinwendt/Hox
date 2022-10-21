module BlackKnight where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


blackKnight = (properties.name .~ "Black Knight") . (properties.manaCost ?~ [CSym (Colored Black),CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ [Protection]) . (properties.typeLine .~ TypeLine [] [Creature] [CType Human,CType Knight]) . (properties.oracleText .~ "First strike (This creature deals combat damage before creatures without first strike.)\nProtection from white (This creature can't be blocked, targeted, dealt damage, or enchanted by anything white.)") . (properties.power .~ (Just (PT 2))) . (properties.toughness .~ (Just (PT 2))) $ defaultCard
