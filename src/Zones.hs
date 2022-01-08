module Zones where
import Card

-- 400.1
data Zones = Library | Hand | Battlefield | Graveyard | Stack | Exile | Command

type Library = [Object]
type Hand = [Object]
type Battlefield = [Object]
type Graveyard = [Object]
type Stack = [Object]
type Exile = [Object]
type Command = [Object]


