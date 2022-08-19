module Farmstead where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


farmstead = (properties.name .~ "Farmstead") . (properties.manaCost ?~ [CSym (Colored White),CSym (Colored White),CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant land\nEnchanted land has \"At the beginning of your upkeep, you may pay {W}{W}. If you do, you gain 1 life.\"") $ defaultCard
