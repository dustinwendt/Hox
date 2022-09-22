module PsionicBlast where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


psionicBlast = (properties.name .~ "Psionic Blast") . (properties.manaCost ?~ [GenSym 2,CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Instant] []) . (properties.oracleText .~ "Psionic Blast deals 4 damage to any target and 2 damage to you.") $ defaultCard
