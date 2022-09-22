module PearledUnicorn where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


pearledUnicorn = (properties.name .~ "Pearled Unicorn") . (properties.manaCost ?~ [GenSym 2,CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Unicorn]) . (properties.power .~ (Just (PT 2))) . (properties.toughness .~ (Just (PT 2))) $ defaultCard
