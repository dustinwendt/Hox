module Colors where

-- 202.2a
data Color = White | Blue | Black | Red | Green deriving (Eq)
data ManaColor = Colored Color | Colorless deriving (Eq)
data Pip = CSym ManaColor | XSym | PhySym | SnowSym | GenSym Int | HyPip Pip Pip deriving (Eq)

instance Show Color where
  show c = case c of
    White -> "W"
    Blue  -> "U"
    Black -> "B"
    Red   -> "R"
    Green -> "G"

instance Show ManaColor where
  show mc = case mc of
    Colored c -> show c
    Colorless -> "C"

instance Show Pip where
  show p = case p of
    HyPip p1 p2 -> show p1 ++ "/" ++ show p2
    CSym c -> show c
    XSym -> "X"
    PhySym -> "P"
    SnowSym -> "S"
    GenSym i -> show i

type PId = Int

