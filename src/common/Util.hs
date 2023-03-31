module Util where

import           Colors
import           ComplexTypes
import           Control.Lens
import           Data.Char
import qualified Data.Map     as M
import           Types

parseColor :: String -> Color
parseColor "W" = White
parseColor "U" = Blue
parseColor "B" = Black
parseColor "R" = Red
parseColor "G" = Green

stripCard :: String -> String
stripCard s =
  let x = filter isAlpha s in
  toLower (head x) : tail x

superTypes go = case go ^. (properties . typeLine) of
  TypeLine x _ _ -> x

cardTypes go = case go ^. (properties . typeLine) of
  TypeLine _ x _ -> x

subTypes go = case go ^. (properties . typeLine) of
  TypeLine _ _ x -> x

isBasic :: GameObject -> Bool
isBasic go = Basic `elem` superTypes go

isSpace :: Char -> Bool
isSpace ' ' = True
isSpace _   = False

validDeck :: M.Map GameObject Int -> Bool
validDeck m = sum (M.elems m) >= 60 && f (M.toList m)
  where f [] = True
        f ((k,v):xs) | not (isBasic k) && v > 4 = False
                     | otherwise = f xs

-- pMC :: String -> [Pip]
-- pMC ('{':'X':'}':xs) = VarPip : pMC xs
-- pMC ('{':'S':'}':xs) = SnowPip : pMC xs
-- pMC ('{':c:'}':xs)
--   | isDigit c = Generic (digitToInt c) : pMC xs
--   | otherwise = ColPip (Colored (parseColor c)) : pMC xs
-- pMC ('{':c:'/':'P':'}':xs) = PhyPip (Colored (parseColor c)) : pMC xs
-- pMC ('{':a:'/':b:'}':xs)
--   | isDigit a = HyPip (Generic (digitToInt a)) (ColPip (Colored (parseColor b))) : pMC xs
--   | otherwise = HyPip (ColPip (Colored (parseColor a))) (ColPip (Colored (parseColor b))) : pMC xs
-- pMC _ = []
