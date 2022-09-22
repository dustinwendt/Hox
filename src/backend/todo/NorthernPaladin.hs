module NorthernPaladin where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


northernPaladin = (properties.name .~ "Northern Paladin") . (properties.manaCost ?~ [GenSym 2,CSym (Colored White),CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Human,CType Knight]) . (properties.oracleText .~ "{W}{W}, {T}: Destroy target black permanent.") . (properties.power .~ (Just (PT 3))) . (properties.toughness .~ (Just (PT 3))) $ defaultCard
