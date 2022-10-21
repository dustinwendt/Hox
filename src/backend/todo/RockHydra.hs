module RockHydra where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


rockHydra = (properties.name .~ "Rock Hydra") . (properties.manaCost ?~ [XSym,CSym (Colored Red),CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Hydra]) . (properties.oracleText .~ "Rock Hydra enters the battlefield with X +1/+1 counters on it.\nFor each 1 damage that would be dealt to Rock Hydra, if it has a +1/+1 counter on it, remove a +1/+1 counter from it and prevent that 1 damage.\n{R}: Prevent the next 1 damage that would be dealt to Rock Hydra this turn.\n{R}{R}{R}: Put a +1/+1 counter on Rock Hydra. Activate only during your upkeep.") . (properties.power .~ (Just (PT 0))) . (properties.toughness .~ (Just (PT 0))) $ defaultCard
