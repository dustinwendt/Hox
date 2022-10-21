module CopperTablet where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


copperTablet = (properties.name .~ "Copper Tablet") . (properties.manaCost ?~ [GenSym 2]) . (properties.color .~ []) . (properties.identity .~ []) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Artifact] []) . (properties.oracleText .~ "At the beginning of each player's upkeep, Copper Tablet deals 1 damage to that player.") $ defaultCard
