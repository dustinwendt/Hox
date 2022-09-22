module BasaltMonolith where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


basaltMonolith = (properties.name .~ "Basalt Monolith") . (properties.manaCost ?~ [GenSym 3]) . (properties.color .~ []) . (properties.identity .~ []) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Artifact] []) . (properties.oracleText .~ "Basalt Monolith doesn't untap during your untap step.\n{T}: Add {C}{C}{C}.\n{3}: Untap Basalt Monolith.") $ defaultCard
