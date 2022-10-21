module Sinkhole where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


sinkhole = (properties.name .~ "Sinkhole") . (properties.manaCost ?~ [CSym (Colored Black),CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Sorcery] []) . (properties.oracleText .~ "Destroy target land.") $ defaultCard
