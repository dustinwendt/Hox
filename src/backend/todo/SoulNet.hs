module SoulNet where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


soulNet = (properties.name .~ "Soul Net") . (properties.manaCost ?~ [GenSym 1]) . (properties.color .~ []) . (properties.identity .~ []) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Artifact] []) . (properties.oracleText .~ "Whenever a creature dies, you may pay {1}. If you do, you gain 1 life.") $ defaultCard
