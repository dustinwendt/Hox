module GiantSpider where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


giantSpider = (properties.name .~ "Giant Spider") . (properties.manaCost ?~ [GenSym 3,CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ [Reach]) . (properties.typeLine .~ TypeLine [] [Creature] [CType Spider]) . (properties.oracleText .~ "Reach (This creature can block creatures with flying.)") . (properties.power .~ (Just (PT 2))) . (properties.toughness .~ (Just (PT 4))) $ defaultCard
