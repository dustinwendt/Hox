module ReverseDamage where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


reverseDamage = (properties.name .~ "Reverse Damage") . (properties.manaCost ?~ [GenSym 1,CSym (Colored White),CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Instant] []) . (properties.oracleText .~ "The next time a source of your choice would deal damage to you this turn, prevent that damage. You gain life equal to the damage prevented this way.") $ defaultCard
