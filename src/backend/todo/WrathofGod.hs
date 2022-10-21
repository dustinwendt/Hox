module WrathofGod where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


wrathofGod = (properties.name .~ "Wrath of God") . (properties.manaCost ?~ [GenSym 2,CSym (Colored White),CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Sorcery] []) . (properties.oracleText .~ "Destroy all creatures. They can't be regenerated.") $ defaultCard
