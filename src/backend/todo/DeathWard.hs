module DeathWard where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


deathWard = (properties.name .~ "Death Ward") . (properties.manaCost ?~ [CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Instant] []) . (properties.oracleText .~ "Regenerate target creature.") $ defaultCard
