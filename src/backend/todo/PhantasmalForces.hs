module PhantasmalForces where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


phantasmalForces = (properties.name .~ "Phantasmal Forces") . (properties.manaCost ?~ [GenSym 3,CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ [Flying]) . (properties.typeLine .~ TypeLine [] [Creature] [CType Illusion]) . (properties.oracleText .~ "Flying\nAt the beginning of your upkeep, sacrifice Phantasmal Forces unless you pay {U}.") . (properties.power .~ (Just (PT 4))) . (properties.toughness .~ (Just (PT 1))) $ defaultCard
