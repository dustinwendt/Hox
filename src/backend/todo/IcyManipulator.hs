module IcyManipulator where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


icyManipulator = (properties.name .~ "Icy Manipulator") . (properties.manaCost ?~ [GenSym 4]) . (properties.color .~ []) . (properties.identity .~ []) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Artifact] []) . (properties.oracleText .~ "{1}, {T}: Tap target artifact, creature, or land.") $ defaultCard
