module KormusBell where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


kormusBell = (properties.name .~ "Kormus Bell") . (properties.manaCost ?~ [GenSym 4]) . (properties.color .~ []) . (properties.identity .~ []) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Artifact] []) . (properties.oracleText .~ "All Swamps are 1/1 black creatures that are still lands.") $ defaultCard
