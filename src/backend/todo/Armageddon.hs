module Armageddon where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


armageddon = (properties.name .~ "Armageddon") . (properties.manaCost ?~ [GenSym 3,CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Sorcery] []) . (properties.oracleText .~ "Destroy all lands.") $ defaultCard
