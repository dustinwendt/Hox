module JadeStatue where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


jadeStatue = (properties.name .~ "Jade Statue") . (properties.manaCost ?~ [GenSym 4]) . (properties.color .~ []) . (properties.identity .~ []) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Artifact] []) . (properties.oracleText .~ "{2}: Jade Statue becomes a 3/6 Golem artifact creature until end of combat. Activate only during combat.") $ defaultCard
