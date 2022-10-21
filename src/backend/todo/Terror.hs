module Terror where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


terror = (properties.name .~ "Terror") . (properties.manaCost ?~ [GenSym 1,CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Instant] []) . (properties.oracleText .~ "Destroy target nonartifact, nonblack creature. It can't be regenerated.") $ defaultCard
