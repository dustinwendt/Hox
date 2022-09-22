module HypnoticSpecter where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


hypnoticSpecter = (properties.name .~ "Hypnotic Specter") . (properties.manaCost ?~ [GenSym 1,CSym (Colored Black),CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ [Flying]) . (properties.typeLine .~ TypeLine [] [Creature] [CType Specter]) . (properties.oracleText .~ "Flying\nWhenever Hypnotic Specter deals damage to an opponent, that player discards a card at random.") . (properties.power .~ (Just (PT 2))) . (properties.toughness .~ (Just (PT 2))) $ defaultCard
