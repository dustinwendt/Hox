module NaturalSelection where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


naturalSelection = (properties.name .~ "Natural Selection") . (properties.manaCost ?~ [CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Instant] []) . (properties.oracleText .~ "Look at the top three cards of target player's library, then put them back in any order. You may have that player shuffle.") $ defaultCard
