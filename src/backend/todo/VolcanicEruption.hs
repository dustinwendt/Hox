module VolcanicEruption where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


volcanicEruption = (properties.name .~ "Volcanic Eruption") . (properties.manaCost ?~ [XSym,CSym (Colored Blue),CSym (Colored Blue),CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Sorcery] []) . (properties.oracleText .~ "Destroy X target Mountains. Volcanic Eruption deals damage to each creature and each player equal to the number of Mountains put into a graveyard this way.") $ defaultCard
