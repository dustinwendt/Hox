module Zones where
import Card

-- 400.1
data Zones = Library | Hand | Battlefield | Graveyard | Stack | Exile | Command

type Library = [Card]
type Graveyard = [Card]
type Command = [Card]
type Exile = [Card]
