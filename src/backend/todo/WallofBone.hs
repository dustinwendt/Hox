module WallofBone where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


wallofBone = (properties.name .~ "Wall of Bone") . (properties.manaCost ?~ [GenSym 2,CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ [Defender]) . (properties.typeLine .~ TypeLine [] [Creature] [CType Skeleton,CType Wall]) . (properties.oracleText .~ "Defender (This creature can't attack.)\n{B}: Regenerate Wall of Bone. (The next time this creature would be destroyed this turn, it isn't. Instead tap it, remove all damage from it, and remove it from combat.)") . (properties.power .~ (Just (PT 1))) . (properties.toughness .~ (Just (PT 4))) $ defaultCard
