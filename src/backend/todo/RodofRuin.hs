module RodofRuin where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


rodofRuin = (properties.name .~ "Rod of Ruin") . (properties.manaCost ?~ [GenSym 4]) . (properties.color .~ []) . (properties.identity .~ []) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Artifact] []) . (properties.oracleText .~ "{3}, {T}: Rod of Ruin deals 1 damage to any target.") $ defaultCard
