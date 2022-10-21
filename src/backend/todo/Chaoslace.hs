module Chaoslace where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


chaoslace = (properties.name .~ "Chaoslace") . (properties.manaCost ?~ [CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Instant] []) . (properties.oracleText .~ "Target spell or permanent becomes red. (Its mana symbols remain unchanged.)") $ defaultCard
