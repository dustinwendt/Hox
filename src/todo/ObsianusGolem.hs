module ObsianusGolem where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


obsianusGolem = (properties.name .~ "Obsianus Golem") . (properties.manaCost ?~ [GenSym 6]) . (properties.color .~ []) . (properties.identity .~ []) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Artifact,Creature] [CType Golem]) . (properties.power .~ (Just (PT 4))) . (properties.toughness .~ (Just (PT 6))) $ defaultCard
