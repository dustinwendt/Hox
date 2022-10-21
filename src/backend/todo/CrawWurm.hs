module CrawWurm where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


crawWurm = (properties.name .~ "Craw Wurm") . (properties.manaCost ?~ [GenSym 4,CSym (Colored Green),CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Wurm]) . (properties.power .~ (Just (PT 6))) . (properties.toughness .~ (Just (PT 4))) $ defaultCard
