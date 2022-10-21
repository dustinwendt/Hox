module RaiseDead where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


raiseDead = (properties.name .~ "Raise Dead") . (properties.manaCost ?~ [CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Sorcery] []) . (properties.oracleText .~ "Return target creature card from your graveyard to your hand.") $ defaultCard
