module Gloom where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


gloom = (properties.name .~ "Gloom") . (properties.manaCost ?~ [GenSym 2,CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] []) . (properties.oracleText .~ "White spells cost {3} more to cast.\nActivated abilities of white enchantments cost {3} more to activate.") $ defaultCard
