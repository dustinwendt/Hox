module BlazeofGlory where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


blazeofGlory = (properties.name .~ "Blaze of Glory") . (properties.manaCost ?~ [CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Instant] []) . (properties.oracleText .~ "Cast this spell only during combat before blockers are declared.\nTarget creature defending player controls can block any number of creatures this turn. It blocks each attacking creature this turn if able.") $ defaultCard
