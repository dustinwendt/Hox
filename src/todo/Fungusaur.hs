module Fungusaur where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


fungusaur = (properties.name .~ "Fungusaur") . (properties.manaCost ?~ [GenSym 3,CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Fungus,CType Dinosaur]) . (properties.oracleText .~ "Whenever Fungusaur is dealt damage, put a +1/+1 counter on it.") . (properties.power .~ (Just (PT 2))) . (properties.toughness .~ (Just (PT 2))) $ defaultCard
