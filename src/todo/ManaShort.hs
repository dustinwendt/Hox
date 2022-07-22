module ManaShort where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


manaShort = (properties.name .~ "Mana Short") . (properties.manaCost ?~ [GenSym 2,CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Instant] []) . (properties.oracleText .~ "Tap all lands target player controls and that player loses all unspent mana.") $ defaultCard
