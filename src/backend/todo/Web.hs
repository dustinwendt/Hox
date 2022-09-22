module Web where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


web = (properties.name .~ "Web") . (properties.manaCost ?~ [CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant creature (Target a creature as you cast this. This card enters the battlefield attached to that creature.)\nEnchanted creature gets +0/+2 and has reach. (It can block creatures with flying.)") $ defaultCard
