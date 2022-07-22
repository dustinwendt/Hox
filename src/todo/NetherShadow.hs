module NetherShadow where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


netherShadow = (properties.name .~ "Nether Shadow") . (properties.manaCost ?~ [CSym (Colored Black),CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ [Haste]) . (properties.typeLine .~ TypeLine [] [Creature] [CType Spirit]) . (properties.oracleText .~ "Haste
At the beginning of your upkeep, if Nether Shadow is in your graveyard with three or more creature cards above it, you may put Nether Shadow onto the battlefield.") . (properties.power .~ (Just (PT 1))) . (properties.toughness .~ (Just (PT 1))) $ defaultCard
