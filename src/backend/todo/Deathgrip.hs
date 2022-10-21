module Deathgrip where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


deathgrip = (properties.name .~ "Deathgrip") . (properties.manaCost ?~ [CSym (Colored Black),CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] []) . (properties.oracleText .~ "{B}{B}: Counter target green spell.") $ defaultCard
