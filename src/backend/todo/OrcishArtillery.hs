module OrcishArtillery where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


orcishArtillery = (properties.name .~ "Orcish Artillery") . (properties.manaCost ?~ [GenSym 1,CSym (Colored Red),CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Orc,CType Warrior]) . (properties.oracleText .~ "{T}: Orcish Artillery deals 2 damage to any target and 3 damage to you.") . (properties.power .~ (Just (PT 1))) . (properties.toughness .~ (Just (PT 3))) $ defaultCard
