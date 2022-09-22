module DrudgeSkeletons where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


drudgeSkeletons = (properties.name .~ "Drudge Skeletons") . (properties.manaCost ?~ [GenSym 1,CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Skeleton]) . (properties.oracleText .~ "{B}: Regenerate Drudge Skeletons. (The next time this creature would be destroyed this turn, it isn't. Instead tap it, remove all damage from it, and remove it from combat.)") . (properties.power .~ (Just (PT 1))) . (properties.toughness .~ (Just (PT 1))) $ defaultCard
