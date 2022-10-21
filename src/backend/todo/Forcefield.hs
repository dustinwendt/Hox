module Forcefield where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


forcefield = (properties.name .~ "Forcefield") . (properties.manaCost ?~ [GenSym 3]) . (properties.color .~ []) . (properties.identity .~ []) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Artifact] []) . (properties.oracleText .~ "{1}: The next time an unblocked creature of your choice would deal combat damage to you this turn, prevent all but 1 of that damage.") $ defaultCard
