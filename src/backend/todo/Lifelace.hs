module Lifelace where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


lifelace = (properties.name .~ "Lifelace") . (properties.manaCost ?~ [CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Instant] []) . (properties.oracleText .~ "Target spell or permanent becomes green. (Mana symbols on that permanent remain unchanged.)") $ defaultCard
