module GuardianAngel where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


guardianAngel = (properties.name .~ "Guardian Angel") . (properties.manaCost ?~ [XSym,CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Instant] []) . (properties.oracleText .~ "Prevent the next X damage that would be dealt to any target this turn. Until end of turn, you may pay {1} any time you could cast an instant. If you do, prevent the next 1 damage that would be dealt to that permanent or player this turn.") $ defaultCard
