module RoyalAssassin where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


royalAssassin = (properties.name .~ "Royal Assassin") . (properties.manaCost ?~ [GenSym 1,CSym (Colored Black),CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Human,CType Assassin]) . (properties.oracleText .~ "{T}: Destroy target tapped creature.") . (properties.power .~ (Just (PT 1))) . (properties.toughness .~ (Just (PT 1))) $ defaultCard
