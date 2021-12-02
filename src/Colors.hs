module Colors where

-- 202.2a
data Color = White | Blue | Black | Red | Green
data ManaColor = Colored Color | Colorless
data Pip = CSym ManaColor | XSym | PhySym | SnowSym | GenSym Int | HyPip Pip Pip

