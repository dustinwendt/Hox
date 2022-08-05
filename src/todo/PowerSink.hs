module PowerSink where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


powerSink = (properties.name .~ "Power Sink") . (properties.manaCost ?~ [XSym,CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Instant] []) . (properties.oracleText .~ "Counter target spell unless its controller pays {X}. If that player doesn't, they tap all lands with mana abilities they control and lose all unspent mana.") $ defaultCard
