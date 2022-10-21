module KeldonWarlord where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


keldonWarlord = (properties.name .~ "Keldon Warlord") . (properties.manaCost ?~ [GenSym 2,CSym (Colored Red),CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Human,CType Barbarian]) . (properties.oracleText .~ "Keldon Warlord's power and toughness are each equal to the number of non-Wall creatures you control.") . (properties.power .~ (Just (Star))) . (properties.toughness .~ (Just (Star))) $ defaultCard
