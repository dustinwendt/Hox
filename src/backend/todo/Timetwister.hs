module Timetwister where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


timetwister = (properties.name .~ "Timetwister") . (properties.manaCost ?~ [GenSym 2,CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Sorcery] []) . (properties.oracleText .~ "Each player shuffles their hand and graveyard into their library, then draws seven cards. (Then put Timetwister into its owner's graveyard.)") $ defaultCard
