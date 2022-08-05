module Sacrifice where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


sacrifice = (properties.name .~ "Sacrifice") . (properties.manaCost ?~ [CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Instant] []) . (properties.oracleText .~ "As an additional cost to cast this spell, sacrifice a creature.
Add an amount of {B} equal to the sacrificed creature's mana value.") $ defaultCard
