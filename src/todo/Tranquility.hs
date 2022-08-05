module Tranquility where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


tranquility = (properties.name .~ "Tranquility") . (properties.manaCost ?~ [GenSym 2,CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Sorcery] []) . (properties.oracleText .~ "Destroy all enchantments.") $ defaultCard
