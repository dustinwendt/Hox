module ClockworkBeast where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


clockworkBeast = (properties.name .~ "Clockwork Beast") . (properties.manaCost ?~ [GenSym 6]) . (properties.color .~ []) . (properties.identity .~ []) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Artifact,Creature] [CType Beast]) . (properties.oracleText .~ "Clockwork Beast enters the battlefield with seven +1/+0 counters on it.\nAt end of combat, if Clockwork Beast attacked or blocked this combat, remove a +1/+0 counter from it.\n{X}, {T}: Put up to X +1/+0 counters on Clockwork Beast. This ability can't cause the total number of +1/+0 counters on Clockwork Beast to be greater than seven. Activate only during your upkeep.") . (properties.power .~ (Just (PT 0))) . (properties.toughness .~ (Just (PT 4))) $ defaultCard
