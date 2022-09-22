module HowlingMine where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


howlingMine = (properties.name .~ "Howling Mine") . (properties.manaCost ?~ [GenSym 2]) . (properties.color .~ []) . (properties.identity .~ []) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Artifact] []) . (properties.oracleText .~ "At the beginning of each player's draw step, if Howling Mine is untapped, that player draws an additional card.") $ defaultCard
