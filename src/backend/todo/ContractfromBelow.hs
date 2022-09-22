module ContractfromBelow where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


contractfromBelow = (properties.name .~ "Contract from Below") . (properties.manaCost ?~ [CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Sorcery] []) . (properties.oracleText .~ "Remove Contract from Below from your deck before playing if you're not playing for ante.\nDiscard your hand, ante the top card of your library, then draw seven cards.") $ defaultCard
