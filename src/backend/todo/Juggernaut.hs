module Juggernaut where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


juggernaut = (properties.name .~ "Juggernaut") . (properties.manaCost ?~ [GenSym 4]) . (properties.color .~ []) . (properties.identity .~ []) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Artifact,Creature] [CType Juggernaut]) . (properties.oracleText .~ "Juggernaut attacks each combat if able.\nJuggernaut can't be blocked by Walls.") . (properties.power .~ (Just (PT 5))) . (properties.toughness .~ (Just (PT 3))) $ defaultCard
