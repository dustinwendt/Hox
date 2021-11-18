module AncestralRecall where

import Data.Maybe
import Types
import Card

ancestralRecall = defaultCard { cardName = "Ancestral Recall"
                              , manaCost = Just [ColPip (Colored Blue)]
                              , color = [Blue]
                              , typeLine = TypeLine [] [Instant] []
                              , textBox = "Target player draws three cards" }
