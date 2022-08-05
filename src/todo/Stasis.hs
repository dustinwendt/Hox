module Stasis where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


stasis = (properties.name .~ "Stasis") . (properties.manaCost ?~ [GenSym 1,CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] []) . (properties.oracleText .~ "Players skip their untap steps.
At the beginning of your upkeep, sacrifice Stasis unless you pay {U}.") $ defaultCard
