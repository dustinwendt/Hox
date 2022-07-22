module Cockatrice where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


cockatrice = (properties.name .~ "Cockatrice") . (properties.manaCost ?~ [GenSym 3,CSym (Colored Green),CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ [Flying]) . (properties.typeLine .~ TypeLine [] [Creature] [CType Cockatrice]) . (properties.oracleText .~ "Flying
Whenever Cockatrice blocks or becomes blocked by a non-Wall creature, destroy that creature at end of combat.") . (properties.power .~ (Just (PT 2))) . (properties.toughness .~ (Just (PT 4))) $ defaultCard
