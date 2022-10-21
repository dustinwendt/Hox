module Lure where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


lure = (properties.name .~ "Lure") . (properties.manaCost ?~ [GenSym 1,CSym (Colored Green),CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant creature\nAll creatures able to block enchanted creature do so.") $ defaultCard
