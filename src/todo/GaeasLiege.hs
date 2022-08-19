module GaeasLiege where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


gaeasLiege = (properties.name .~ "Gaea's Liege") . (properties.manaCost ?~ [GenSym 3,CSym (Colored Green),CSym (Colored Green),CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Avatar]) . (properties.oracleText .~ "As long as Gaea's Liege isn't attacking, its power and toughness are each equal to the number of Forests you control. As long as Gaea's Liege is attacking, its power and toughness are each equal to the number of Forests defending player controls.\n{T}: Target land becomes a Forest until Gaea's Liege leaves the battlefield.") . (properties.power .~ (Just (Star))) . (properties.toughness .~ (Just (Star))) $ defaultCard
