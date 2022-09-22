module ChaosOrb where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


chaosOrb = (properties.name .~ "Chaos Orb") . (properties.manaCost ?~ [GenSym 2]) . (properties.color .~ []) . (properties.identity .~ []) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Artifact] []) . (properties.oracleText .~ "{1}, {T}: If Chaos Orb is on the battlefield, flip Chaos Orb onto the battlefield from a height of at least one foot. If Chaos Orb turns over completely at least once during the flip, destroy all nontoken permanents it touches. Then destroy Chaos Orb.") $ defaultCard
