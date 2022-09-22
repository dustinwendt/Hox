module DrainPower where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


drainPower = (properties.name .~ "Drain Power") . (properties.manaCost ?~ [CSym (Colored Blue),CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Sorcery] []) . (properties.oracleText .~ "Target player activates a mana ability of each land they control. Then that player loses all unspent mana and you add the mana lost this way.") $ defaultCard
