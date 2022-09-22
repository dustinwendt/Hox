module StealArtifact where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


stealArtifact = (properties.name .~ "Steal Artifact") . (properties.manaCost ?~ [GenSym 2,CSym (Colored Blue),CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant artifact\nYou control enchanted artifact.") $ defaultCard
