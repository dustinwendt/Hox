module HowlfromBeyond where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


howlfromBeyond = (properties.name .~ "Howl from Beyond") . (properties.manaCost ?~ [XSym,CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Instant] []) . (properties.oracleText .~ "Target creature gets +X/+0 until end of turn.") $ defaultCard
