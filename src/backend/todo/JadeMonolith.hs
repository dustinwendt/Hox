module JadeMonolith where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


jadeMonolith = (properties.name .~ "Jade Monolith") . (properties.manaCost ?~ [GenSym 4]) . (properties.color .~ []) . (properties.identity .~ []) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Artifact] []) . (properties.oracleText .~ "{1}: The next time a source of your choice would deal damage to target creature this turn, that source deals that damage to you instead.") $ defaultCard
