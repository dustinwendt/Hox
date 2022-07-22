module HealingSalve where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


healingSalve = (properties.name .~ "Healing Salve") . (properties.manaCost ?~ [CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Instant] []) . (properties.oracleText .~ "Choose one —
• Target player gains 3 life.
• Prevent the next 3 damage that would be dealt to any target this turn.") $ defaultCard
