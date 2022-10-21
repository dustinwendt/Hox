module Resurrection where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


resurrection = (properties.name .~ "Resurrection") . (properties.manaCost ?~ [GenSym 2,CSym (Colored White),CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Sorcery] []) . (properties.oracleText .~ "Return target creature card from your graveyard to the battlefield.") $ defaultCard
