module OrcishOriflamme where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


orcishOriflamme = (properties.name .~ "Orcish Oriflamme") . (properties.manaCost ?~ [GenSym 3,CSym (Colored Red)]) . (properties.color .~ [Red]) . (properties.identity .~ [Red]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] []) . (properties.oracleText .~ "Attacking creatures you control get +1/+0.") $ defaultCard
