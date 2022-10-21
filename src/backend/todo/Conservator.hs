module Conservator where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


conservator = (properties.name .~ "Conservator") . (properties.manaCost ?~ [GenSym 4]) . (properties.color .~ []) . (properties.identity .~ []) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Artifact] []) . (properties.oracleText .~ "{3}, {T}: Prevent the next 2 damage that would be dealt to you this turn.") $ defaultCard
