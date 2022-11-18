module AncestralRecall where

import           Colors
import           ComplexTypes
import           Control.Lens
import           Data.Maybe
import           Types


ancestralRecall = (properties.name .~ "Ancestral Recall") . (properties.manaCost ?~ [CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Instant] []) . (properties.oracleText .~ "Target player draws three cards.")
                           $ defaultCard
