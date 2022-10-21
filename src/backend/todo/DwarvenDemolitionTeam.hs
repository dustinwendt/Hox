module DwarvenDemolitionTeam where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


dwarvenDemolitionTeam = (properties.name .~ "Dwarven Demolition Team") . (properties.manaCost ?~ [GenSym 2,CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Dwarf]) . (properties.oracleText .~ "{T}: Destroy target Wall.") . (properties.power .~ (Just (PT 1))) . (properties.toughness .~ (Just (PT 1))) $ defaultCard
