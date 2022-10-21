module ManaFlare where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


manaFlare = (properties.name .~ "Mana Flare") . (properties.manaCost ?~ [GenSym 2,CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] []) . (properties.oracleText .~ "Whenever a player taps a land for mana, that player adds one mana of any type that land produced.") $ defaultCard
