module RedWard where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


redWard = (properties.name .~ "Red Ward") . (properties.manaCost ?~ [CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant creature\nEnchanted creature has protection from red. This effect doesn't remove Red Ward.") $ defaultCard
