module LivingArtifact where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


livingArtifact = (properties.name .~ "Living Artifact") . (properties.manaCost ?~ [CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant artifact
Whenever you're dealt damage, put that many vitality counters on Living Artifact.
At the beginning of your upkeep, you may remove a vitality counter from Living Artifact. If you do, you gain 1 life.") $ defaultCard
