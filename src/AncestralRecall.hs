module AncestralRecall where

import Data.Maybe
import Colors
import Types
import Card

ancestralRecall = defaultCard { cardName = "Ancestral Recall"
                              , manaCost = [CSym (Colored Blue)]
                              , color = [Blue]
                              , typeLine = TypeLine [] [Instant] []
                              , textBox = "Target player draws three cards" }
