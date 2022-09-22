module GrizzlyBears where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


grizzlyBears = (properties.name .~ "Grizzly Bears") . (properties.manaCost ?~ [GenSym 1,CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Bear]) . (properties.power .~ (Just (PT 2))) . (properties.toughness .~ (Just (PT 2))) $ defaultCard
