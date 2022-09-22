module IslandSanctuary where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


islandSanctuary = (properties.name .~ "Island Sanctuary") . (properties.manaCost ?~ [GenSym 1,CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] []) . (properties.oracleText .~ "If you would draw a card during your draw step, instead you may skip that draw. If you do, until your next turn, you can't be attacked except by creatures with flying and/or islandwalk.") $ defaultCard
