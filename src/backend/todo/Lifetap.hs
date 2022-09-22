module Lifetap where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


lifetap = (properties.name .~ "Lifetap") . (properties.manaCost ?~ [CSym (Colored Blue),CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] []) . (properties.oracleText .~ "Whenever a Forest an opponent controls becomes tapped, you gain 1 life.") $ defaultCard
