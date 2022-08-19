module PersonalIncarnation where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


personalIncarnation = (properties.name .~ "Personal Incarnation") . (properties.manaCost ?~ [GenSym 3,CSym (Colored White),CSym (Colored White),CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Avatar,CType Incarnation]) . (properties.oracleText .~ "{0}: The next 1 damage that would be dealt to Personal Incarnation this turn is dealt to its owner instead. Only Personal Incarnation's owner may activate this ability.\nWhen Personal Incarnation dies, its owner loses half their life, rounded up.") . (properties.power .~ (Just (PT 6))) . (properties.toughness .~ (Just (PT 6))) $ defaultCard
