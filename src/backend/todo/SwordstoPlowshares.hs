module SwordstoPlowshares where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


swordstoPlowshares = (properties.name .~ "Swords to Plowshares") . (properties.manaCost ?~ [CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Instant] []) . (properties.oracleText .~ "Exile target creature. Its controller gains life equal to its power.") $ defaultCard
