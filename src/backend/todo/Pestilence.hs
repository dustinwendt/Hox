module Pestilence where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


pestilence = (properties.name .~ "Pestilence") . (properties.manaCost ?~ [GenSym 2,CSym (Colored Black),CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] []) . (properties.oracleText .~ "At the beginning of the end step, if no creatures are on the battlefield, sacrifice Pestilence.\n{B}: Pestilence deals 1 damage to each creature and each player.") $ defaultCard
