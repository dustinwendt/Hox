{-# LANGUAGE TemplateHaskell #-}
module Card where

import Colors
import Control.Lens hiding (flipped)
import Control.Exception
import Types

-- 208 Power/Toughness
data PT = Star | StarPlus Int | PT Int deriving (Eq)
-- data Power = PowStar StarNum | PowNum Int
-- data Toughness = TouStar StarNum | TouNum Int

data Legality = Legality { standard :: Bool
                         , modern :: Bool
                         , legacy :: Bool
                         , vintage :: Bool
                         } deriving (Eq)

$(makeLenses ''Legality)

data Status =
  Status { _tapped :: Bool
         , _flipped :: Bool
         , _faceUp :: Bool
         , _phased :: Bool} deriving Eq

$(makeLenses ''Status)

defaultStatus =
  Status { _tapped = False
         , _flipped = False
         , _faceUp = True
         , _phased = False}


data Properties = Properties
  { _name :: String
  , _manaCost :: [Pip]
  , _color :: [Color]
  , _identity :: [Color]
  , _typeLine :: TypeLine
  , _textBox :: String
  , _power :: Maybe PT
  , _toughness :: Maybe PT
  , _loyalty :: Maybe Int
  , _legality :: Legality
  , _owner :: Int
  , _controller :: Int
  } deriving Eq

$(makeLenses ''Properties)

data ObjectType = Ability | Card | Copy | Token | Spell (Maybe Object) | Permanent (Maybe Object) Status | Emblem deriving (Eq)

data Object =
  Object { _properties :: Properties
         , _object :: ObjectType} deriving (Eq)

$(makeLenses ''Object)

isSpell :: Object -> Bool
isSpell o = let t = o^.object in
  t == Card || t == Copy

copySpell s = assert (isSpell s) $ object .~ Copy $ s

isPermanent :: Object -> Bool
isPermanent p = let t = p ^. object in
  t == Card || t == Token

defaultLegality = Legality { standard = True
                           , modern   = True
                           , legacy   = True
                           , vintage  = True
                           }

defaultProperties :: Properties
defaultProperties = Properties
  { _name = "DefaultCard"
  , _manaCost = []
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
manaValue o = foldr ((+) . pipValue) 0 (o^.properties.manaCost)

isMonoColored :: Object -> Bool
isMonoColored o = length (o^.properties.color) == 1

isMultiColored :: Object -> Bool
isMultiColored o = length (o^.properties.color) > 1

isColorless :: Object -> Bool
isColorless o = null (o^.properties.color)

