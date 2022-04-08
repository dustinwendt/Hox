{-# LANGUAGE TemplateHaskell #-}
module Card where

import Colors
import Control.Lens hiding (flipped)
import Control.Exception
import Types

-- 208 Power/Toughness
data PT = Star | StarPlus Int | PT Int deriving (Eq)

instance Show PT where
  show Star         = "*"
  show (StarPlus i) = "*+" ++ show i
  show (PT i)       = show i

-- data Power = PowStar StarNum | PowNum Int
-- data Toughness = TouStar StarNum | TouNum Int

data Legality = Legality { standard :: Bool
                         , modern :: Bool
                         , legacy :: Bool
                         , vintage :: Bool
                         } deriving (Eq)

$(makeLenses ''Legality)

-- 110.5
data Status =
  Status { _tapped :: Bool
         , _flipped :: Bool
         , _faceUp :: Bool
         , _phased :: Bool
         , _sick :: Bool} deriving (Eq, Show)

$(makeLenses ''Status)

-- 110.5b
defaultStatus =
  Status { _tapped = False
         , _flipped = False
         , _faceUp = True
         , _phased = False
         , _sick = True }

-- 109.3
data Properties = Properties
  { _name :: String
  , _manaCost :: Maybe [Pip]
  , _color :: [Color]
  , _identity :: [Color]
  , _typeLine :: TypeLine
  , _textBox :: String
  , _power :: Maybe PT
  , _toughness :: Maybe PT
  , _loyalty :: Maybe Int
  , _legality :: Legality
  , _owner :: PId
  , _controller :: PId
  } deriving (Eq)

$(makeLenses ''Properties)

instance Show Properties where
  show p = show (p^.name) ++ " " ++ mc (p^.manaCost) ++ "\n" ++
           tl (p^.typeLine) ++
           tb (p^.textBox) ++
           f (p^.power, p^.toughness)
   where f (Just x, Just y) = show x ++ "/" ++ show y ++ "\n"
         f _ = ""
         g [] = ""
         g (x:xs) = "{" ++ show x ++ "}" ++ g xs
         tl (TypeLine [] [] []) = ""
         tl typeline            = show typeline ++ "\n"
         tb textbox = if null textbox then "" else textbox ++ "\n"
         mc Nothing = ""
         mc (Just p) = g p


-- 109.1
-- Token | Copy are represented by Nothing being passed to Permanent and Spell respectively
data ObjectType = Ability | Card | Spell (Maybe Object) | Permanent (Maybe Object) Status | Emblem deriving (Eq)

data Object =
  Object { _properties :: Properties
         , _object :: ObjectType} deriving Eq

$(makeLenses ''Object)

instance Show Object where
  show o = show $ o^.properties

isCard o = o^.object == Card

isSpell o = let t = o^.object in
  case t of
    Spell _ -> True
    _       -> False

copySpell :: Object -> Object
copySpell s = assert (isSpell s) $ object .~ Spell Nothing $ s

-- isPermanent :: Object -> Bool
-- isPermanent p = let t = p ^. object in
--   t == Card || t == Token

defaultLegality = Legality { standard = True
                           , modern   = True
                           , legacy   = True
                           , vintage  = True
                           }

defaultProperties :: Properties
defaultProperties = Properties
  { _name = "DefaultCard"
  , _manaCost = Nothing
  , _color = []
  , _identity = []
  , _typeLine = TypeLine [] [] []
  , _textBox = ""
  , _power = Nothing
  , _toughness = Nothing
  , _loyalty = Nothing
  , _legality = defaultLegality
  , _owner = -1
  , _controller = -1
  }

defaultCard =
  Object { _properties = defaultProperties
         , _object = Card}

-- 202.3
pipValue :: Pip -> Int
pipValue x = case x of
              HyPip a b -> if v a > v b then v a else v b
              p -> v p
           where v XSym       = 0
                 v (GenSym i) = i
                 v _          = 1

manaValue :: Object -> Int
manaValue o = case o^.properties.manaCost of
  Just pips -> foldr ((+) . pipValue) 0 pips

isMonoColored :: Object -> Bool
isMonoColored o = length (o^.properties.color) == 1

isMultiColored :: Object -> Bool
isMultiColored o = length (o^.properties.color) > 1

isColorless :: Object -> Bool
isColorless o = null (o^.properties.color)

