module Kudzu where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


kudzu = (properties.name .~ "Kudzu") . (properties.manaCost ?~ [GenSym 1,CSym (Colored Green),CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant land
When enchanted land becomes tapped, destroy it. That land's controller may attach Kudzu to a land of their choice.") $ defaultCard
