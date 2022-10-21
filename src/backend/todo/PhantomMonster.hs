module PhantomMonster where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


phantomMonster = (properties.name .~ "Phantom Monster") . (properties.manaCost ?~ [GenSym 3,CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ [Flying]) . (properties.typeLine .~ TypeLine [] [Creature] [CType Illusion]) . (properties.oracleText .~ "Flying") . (properties.power .~ (Just (PT 3))) . (properties.toughness .~ (Just (PT 3))) $ defaultCard
