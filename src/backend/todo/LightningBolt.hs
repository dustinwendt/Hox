module LightningBolt where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


lightningBolt = (properties.name .~ "Lightning Bolt") . (properties.manaCost ?~ [CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Instant] []) . (properties.oracleText .~ "Lightning Bolt deals 3 damage to any target.") $ defaultCard
