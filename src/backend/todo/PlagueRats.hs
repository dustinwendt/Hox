module PlagueRats where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


plagueRats = (properties.name .~ "Plague Rats") . (properties.manaCost ?~ [GenSym 2,CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Rat]) . (properties.oracleText .~ "Plague Rats's power and toughness are each equal to the number of creatures named Plague Rats on the battlefield.") . (properties.power .~ (Just (Star))) . (properties.toughness .~ (Just (Star))) $ defaultCard
