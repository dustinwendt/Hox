module Tunnel where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


tunnel = (properties.name .~ "Tunnel") . (properties.manaCost ?~ [CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Instant] []) . (properties.oracleText .~ "Destroy target Wall. It can't be regenerated.") $ defaultCard
