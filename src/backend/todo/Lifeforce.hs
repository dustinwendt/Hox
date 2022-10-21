module Lifeforce where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


lifeforce = (properties.name .~ "Lifeforce") . (properties.manaCost ?~ [CSym (Colored Green),CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] []) . (properties.oracleText .~ "{G}{G}: Counter target black spell.") $ defaultCard
