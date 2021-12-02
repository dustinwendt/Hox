module Basics.Basics where

import Card
import Types

plains = defaultCard { cardName = "Plains"
                     , typeLine = TypeLine [Basic] [Land] [LType Plains]}

island = defaultCard { cardName = "Island"
                     , typeLine = TypeLine [Basic] [Land] [LType Island]}

swamp = defaultCard { cardName = "Swamp"
                    , typeLine = TypeLine [Basic] [Land] [LType Swamp]}

mountain = defaultCard { cardName = "Mountain"
                       , typeLine = TypeLine [Basic] [Land] [LType Mountain]}

forest = defaultCard { cardName = "Forest"
                     , typeLine = TypeLine [Basic] [Land] [LType Forest]}
 
