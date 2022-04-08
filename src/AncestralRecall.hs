module AncestralRecall where

import Data.Maybe
import Colors
import Control.Lens
import Types
import Card

ancestralRecall =   (properties.name .~ "Ancestrall Recall")
                  . (properties.manaCost ?~ [CSym (Colored Blue)])
                  . (properties.color .~ [Blue])
                  . (properties.typeLine .~ TypeLine [] [Instant] [])
                  . (properties.textBox .~ "Target player draws three cards")
                  -- . (properties.legality.legacy .~ False)
                  $ defaultCard
