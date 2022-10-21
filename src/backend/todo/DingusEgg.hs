module DingusEgg where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


dingusEgg = (properties.name .~ "Dingus Egg") . (properties.manaCost ?~ [GenSym 4]) . (properties.color .~ []) . (properties.identity .~ []) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Artifact] []) . (properties.oracleText .~ "Whenever a land is put into a graveyard from the battlefield, Dingus Egg deals 2 damage to that land's controller.") $ defaultCard
