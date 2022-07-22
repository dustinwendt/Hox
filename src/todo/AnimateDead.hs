module AnimateDead where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


animateDead = (properties.name .~ "Animate Dead") . (properties.manaCost ?~ [GenSym 1,CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant creature card in a graveyard
When Animate Dead enters the battlefield, if it's on the battlefield, it loses "enchant creature card in a graveyard" and gains "enchant creature put onto the battlefield with Animate Dead." Return enchanted creature card to the battlefield under your control and attach Animate Dead to it. When Animate Dead leaves the battlefield, that creature's controller sacrifices it.
Enchanted creature gets -1/-0.") $ defaultCard
