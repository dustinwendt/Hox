module Meekstone where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


meekstone = (properties.name .~ "Meekstone") . (properties.manaCost ?~ [GenSym 1]) . (properties.color .~ []) . (properties.identity .~ []) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Artifact] []) . (properties.oracleText .~ "Creatures with power 3 or greater don't untap during their controllers' untap steps.") $ defaultCard
