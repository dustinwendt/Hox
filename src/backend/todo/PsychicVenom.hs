module PsychicVenom where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


psychicVenom = (properties.name .~ "Psychic Venom") . (properties.manaCost ?~ [GenSym 1,CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant land\nWhenever enchanted land becomes tapped, Psychic Venom deals 2 damage to that land's controller.") $ defaultCard
