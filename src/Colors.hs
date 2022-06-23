{-# LANGUAGE DeriveGeneric #-}
module Colors where

import Data.Aeson
import Data.Map hiding (foldl, foldr, map)
import GHC.Generics

-- 202.2a
data Color = White | Blue | Black | Red | Green deriving (Eq, Generic, Ord)
data Mana = Colored Color | Colorless deriving (Eq, Ord)
data Pip = CSym Mana | XSym | PhySym | SnowSym | GenSym Int | HyPip Pip Pip deriving (Eq, Generic)

pools :: [Mana]
pools = Colorless : [ Colored c | c <- colors]
  where colors = [White, Blue, Black, Red, Green]

instance Show Color where
  show c = case c of
    White -> "W"
    Blue  -> "U"
    Black -> "B"
    Red   -> "R"
    Green -> "G"

colorString :: String -> Color
colorString s = case s of
  "W" -> White
  "U" -> Blue
  "B" -> Black
  "R" -> Red
  "G" -> Green

instance Show Mana where
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

type ManaPool = Map Mana Int

emptyPool :: ManaPool
emptyPool = foldl (\ x p -> insert p 0 x) empty pools
