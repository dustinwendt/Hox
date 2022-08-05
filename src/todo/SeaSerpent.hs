module SeaSerpent where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


seaSerpent = (properties.name .~ "Sea Serpent") . (properties.manaCost ?~ [GenSym 5,CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Serpent]) . (properties.oracleText .~ "Sea Serpent can't attack unless defending player controls an Island.
When you control no Islands, sacrifice Sea Serpent.") . (properties.power .~ (Just (PT 5))) . (properties.toughness .~ (Just (PT 5))) $ defaultCard
