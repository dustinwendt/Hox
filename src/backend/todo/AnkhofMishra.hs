module AnkhofMishra where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


ankhofMishra = (properties.name .~ "Ankh of Mishra") . (properties.manaCost ?~ [GenSym 2]) . (properties.color .~ []) . (properties.identity .~ []) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Artifact] []) . (properties.oracleText .~ "Whenever a land enters the battlefield, Ankh of Mishra deals 2 damage to that land's controller.") $ defaultCard
