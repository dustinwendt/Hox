module VesuvanDoppelganger where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


vesuvanDoppelganger = (properties.name .~ "Vesuvan Doppelganger") . (properties.manaCost ?~ [GenSym 3,CSym (Colored Blue),CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Shapeshifter]) . (properties.oracleText .~ "You may have Vesuvan Doppelganger enter the battlefield as a copy of any creature on the battlefield, except it doesn't copy that creature's color and it has \"At the beginning of your upkeep, you may have this creature become a copy of target creature, except it doesn't copy that creature's color and it has this ability.\"") . (properties.power .~ (Just (PT 0))) . (properties.toughness .~ (Just (PT 0))) $ defaultCard
