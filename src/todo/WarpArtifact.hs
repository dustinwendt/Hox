module WarpArtifact where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


warpArtifact = (properties.name .~ "Warp Artifact") . (properties.manaCost ?~ [CSym (Colored Black),CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant artifact
At the beginning of the upkeep of enchanted artifact's controller, Warp Artifact deals 1 damage to that player.") $ defaultCard
