module Util where

import Prelude hiding (reverse)
import Data.Char
import Card
import Types

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

parseColor :: Char -> Color
parseColor 'W' = White
parseColor 'U' = Blue
parseColor 'B' = Black
parseColor 'R' = Red
parseColor 'G' = Green

pMC :: String -> [Pip]
pMC ('{':'X':'}':xs) = VarPip : pMC xs
pMC ('{':'S':'}':xs) = SnowPip : pMC xs
pMC ('{':c:'}':xs)
  | isDigit c = Generic (digitToInt c) : pMC xs
  | otherwise = ColPip (Colored (parseColor c)) : pMC xs
pMC ('{':c:'/':'P':'}':xs) = PhyPip (Colored (parseColor c)) : pMC xs
pMC ('{':a:'/':b:'}':xs)
  | isDigit a = HyPip (Generic (digitToInt a)) (ColPip (Colored (parseColor b))) : pMC xs
  | otherwise = HyPip (ColPip (Colored (parseColor a))) (ColPip (Colored (parseColor b))) : pMC xs
pMC _ = []
