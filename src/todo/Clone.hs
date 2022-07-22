module Clone where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


clone = (properties.name .~ "Clone") . (properties.manaCost ?~ [GenSym 3,CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Shapeshifter]) . (properties.oracleText .~ "You may have Clone enter the battlefield as a copy of any creature on the battlefield.") . (properties.power .~ (Just (PT 0))) . (properties.toughness .~ (Just (PT 0))) $ defaultCard
