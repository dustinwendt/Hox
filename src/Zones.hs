module Zones where
import Card

-- 400.1
data Zones = Library | Hand | Battlefield | Graveyard | Stack | Exile | Command

type Library = [GameObject]
type Hand = [GameObject]
type Battlefield = [GameObject]
type Graveyard = [GameObject]
type Stack = [GameObject]
type Exile = [GameObject]
type Command = [GameObject]


