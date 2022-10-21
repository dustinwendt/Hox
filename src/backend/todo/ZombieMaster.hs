module ZombieMaster where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


zombieMaster = (properties.name .~ "Zombie Master") . (properties.manaCost ?~ [GenSym 1,CSym (Colored Black),CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Zombie]) . (properties.oracleText .~ "Other Zombie creatures have swampwalk. (They can't be blocked as long as defending player controls a Swamp.)\nOther Zombies have \"{B}: Regenerate this permanent.\"") . (properties.power .~ (Just (PT 2))) . (properties.toughness .~ (Just (PT 3))) $ defaultCard
