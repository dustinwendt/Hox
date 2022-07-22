module DisruptingScepter where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


disruptingScepter = (properties.name .~ "Disrupting Scepter") . (properties.manaCost ?~ [GenSym 3]) . (properties.color .~ []) . (properties.identity .~ []) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Artifact] []) . (properties.oracleText .~ "{3}, {T}: Target player discards a card. Activate only during your turn.") $ defaultCard
