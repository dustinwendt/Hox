module Channel where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


channel = (properties.name .~ "Channel") . (properties.manaCost ?~ [CSym (Colored Green),CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Sorcery] []) . (properties.oracleText .~ "Until end of turn, any time you could activate a mana ability, you may pay 1 life. If you do, add {C}.") $ defaultCard
