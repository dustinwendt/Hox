module Colors where

-- 202.2a
data Color = White | Blue | Black | Red | Green deriving (Eq)
data ManaColor = Colored Color | Colorless deriving (Eq)
data Pip = CSym ManaColor | XSym | PhySym | SnowSym | GenSym Int | HyPip Pip Pip deriving (Eq)

