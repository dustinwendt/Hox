module ProdigalSorcerer where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


prodigalSorcerer = (properties.name .~ "Prodigal Sorcerer") . (properties.manaCost ?~ [GenSym 2,CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Human,CType Wizard]) . (properties.oracleText .~ "{T}: Prodigal Sorcerer deals 1 damage to any target.") . (properties.power .~ (Just (PT 1))) . (properties.toughness .~ (Just (PT 1))) $ defaultCard
