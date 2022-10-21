module GauntletofMight where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


gauntletofMight = (properties.name .~ "Gauntlet of Might") . (properties.manaCost ?~ [GenSym 4]) . (properties.color .~ []) . (properties.identity .~ [Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Artifact] []) . (properties.oracleText .~ "Red creatures get +1/+1.\nWhenever a Mountain is tapped for mana, its controller adds an additional {R}.") $ defaultCard
