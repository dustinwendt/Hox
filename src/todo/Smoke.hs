module Smoke where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


smoke = (properties.name .~ "Smoke") . (properties.manaCost ?~ [CSym (Colored Red),CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] []) . (properties.oracleText .~ "Players can't untap more than one creature during their untap steps.") $ defaultCard
