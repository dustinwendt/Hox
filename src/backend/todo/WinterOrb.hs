module WinterOrb where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


winterOrb = (properties.name .~ "Winter Orb") . (properties.manaCost ?~ [GenSym 2]) . (properties.color .~ []) . (properties.identity .~ []) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Artifact] []) . (properties.oracleText .~ "As long as Winter Orb is untapped, players can't untap more than one land during their untap steps.") $ defaultCard
