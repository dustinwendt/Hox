module ScavengingGhoul where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


scavengingGhoul = (properties.name .~ "Scavenging Ghoul") . (properties.manaCost ?~ [GenSym 3,CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Zombie]) . (properties.oracleText .~ "At the beginning of each end step, put a corpse counter on Scavenging Ghoul for each creature that died this turn.\nRemove a corpse counter from Scavenging Ghoul: Regenerate Scavenging Ghoul.") . (properties.power .~ (Just (PT 2))) . (properties.toughness .~ (Just (PT 2))) $ defaultCard
