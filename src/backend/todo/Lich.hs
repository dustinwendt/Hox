module Lich where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


lich = (properties.name .~ "Lich") . (properties.manaCost ?~ [CSym (Colored Black),CSym (Colored Black),CSym (Colored Black),CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] []) . (properties.oracleText .~ "As Lich enters the battlefield, you lose life equal to your life total.\nYou don't lose the game for having 0 or less life.\nIf you would gain life, draw that many cards instead.\nWhenever you're dealt damage, sacrifice that many nontoken permanents. If you can't, you lose the game.\nWhen Lich is put into a graveyard from the battlefield, you lose the game.") $ defaultCard
