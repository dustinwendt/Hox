module VeteranBodyguard where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


veteranBodyguard = (properties.name .~ "Veteran Bodyguard") . (properties.manaCost ?~ [GenSym 3,CSym (Colored White),CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Human]) . (properties.oracleText .~ "As long as Veteran Bodyguard is untapped, all damage that would be dealt to you by unblocked creatures is dealt to Veteran Bodyguard instead.") . (properties.power .~ (Just (PT 2))) . (properties.toughness .~ (Just (PT 5))) $ defaultCard
