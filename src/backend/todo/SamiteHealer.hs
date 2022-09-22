module SamiteHealer where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


samiteHealer = (properties.name .~ "Samite Healer") . (properties.manaCost ?~ [GenSym 1,CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Human,CType Cleric]) . (properties.oracleText .~ "{T}: Prevent the next 1 damage that would be dealt to any target this turn.") . (properties.power .~ (Just (PT 1))) . (properties.toughness .~ (Just (PT 1))) $ defaultCard
