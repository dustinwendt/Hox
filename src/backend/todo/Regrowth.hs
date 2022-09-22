module Regrowth where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


regrowth = (properties.name .~ "Regrowth") . (properties.manaCost ?~ [GenSym 1,CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Sorcery] []) . (properties.oracleText .~ "Return target card from your graveyard to your hand.") $ defaultCard
