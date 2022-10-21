module Darkpact where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


darkpact = (properties.name .~ "Darkpact") . (properties.manaCost ?~ [CSym (Colored Black),CSym (Colored Black),CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Sorcery] []) . (properties.oracleText .~ "Remove Darkpact from your deck before playing if you're not playing for ante.\nYou own target card in the ante. Exchange that card with the top card of your library.") $ defaultCard
