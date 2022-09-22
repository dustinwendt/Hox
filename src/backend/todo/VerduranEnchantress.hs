module VerduranEnchantress where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


verduranEnchantress = (properties.name .~ "Verduran Enchantress") . (properties.manaCost ?~ [GenSym 1,CSym (Colored Green),CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Human,CType Druid]) . (properties.oracleText .~ "Whenever you cast an enchantment spell, you may draw a card.") . (properties.power .~ (Just (PT 0))) . (properties.toughness .~ (Just (PT 2))) $ defaultCard
