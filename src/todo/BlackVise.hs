module BlackVise where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


blackVise = (properties.name .~ "Black Vise") . (properties.manaCost ?~ [GenSym 1]) . (properties.color .~ []) . (properties.identity .~ []) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Artifact] []) . (properties.oracleText .~ "As Black Vise enters the battlefield, choose an opponent.
At the beginning of the chosen player's upkeep, Black Vise deals X damage to that player, where X is the number of cards in their hand minus 4.") $ defaultCard
