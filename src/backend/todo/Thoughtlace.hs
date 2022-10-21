module Thoughtlace where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


thoughtlace = (properties.name .~ "Thoughtlace") . (properties.manaCost ?~ [CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Instant] []) . (properties.oracleText .~ "Target spell or permanent becomes blue. (Mana symbols on that permanent remain unchanged.)") $ defaultCard
