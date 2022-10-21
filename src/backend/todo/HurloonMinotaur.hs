module HurloonMinotaur where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


hurloonMinotaur = (properties.name .~ "Hurloon Minotaur") . (properties.manaCost ?~ [GenSym 1,CSym (Colored Red),CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Minotaur]) . (properties.power .~ (Just (PT 2))) . (properties.toughness .~ (Just (PT 3))) $ defaultCard
