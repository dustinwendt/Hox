module MindTwist where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


mindTwist = (properties.name .~ "Mind Twist") . (properties.manaCost ?~ [XSym,CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Sorcery] []) . (properties.oracleText .~ "Target player discards X cards at random.") $ defaultCard
