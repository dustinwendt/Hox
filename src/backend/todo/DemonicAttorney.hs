module DemonicAttorney where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


demonicAttorney = (properties.name .~ "Demonic Attorney") . (properties.manaCost ?~ [GenSym 1,CSym (Colored Black),CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Sorcery] []) . (properties.oracleText .~ "Remove Demonic Attorney from your deck before playing if you're not playing for ante.\nEach player antes the top card of their library.") $ defaultCard
