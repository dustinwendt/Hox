module DemonicTutor where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


demonicTutor = (properties.name .~ "Demonic Tutor") . (properties.manaCost ?~ [GenSym 1,CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Sorcery] []) . (properties.oracleText .~ "Search your library for a card, put that card into your hand, then shuffle.") $ defaultCard
