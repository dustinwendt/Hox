module GlassesofUrza where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


glassesofUrza = (properties.name .~ "Glasses of Urza") . (properties.manaCost ?~ [GenSym 1]) . (properties.color .~ []) . (properties.identity .~ []) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Artifact] []) . (properties.oracleText .~ "{T}: Look at target player's hand.") $ defaultCard
