module Fear where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


fear = (properties.name .~ "Fear") . (properties.manaCost ?~ [CSym (Colored Black),CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant creature (Target a creature as you cast this. This card enters the battlefield attached to that creature.)\nEnchanted creature has fear. (It can't be blocked except by artifact creatures and/or black creatures.)") $ defaultCard
