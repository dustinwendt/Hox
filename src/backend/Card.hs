{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Card where

import           Colors
import           Control.Exception
import           Control.Lens      hiding (flipped)
import           Data.Char
-- import {-# SOURCE #-}           Engine
import           GHC.Generics
import           Types

-- 208 Power/Toughness
data PT = Star | StarPlus Int | PT Int deriving (Eq)

data Keyword = Banding | Defender | FirstStrike | Fear | Flying | Haste | Indestructible | LandWalk | Protection | Reach | Regeneration | Trample | Vigilance deriving (Enum, Eq, Generic, Show)

instance Show PT where
  show Star         = "*"
  show (StarPlus i) = "*+" ++ show i
  show (PT i)       = show i

-- data Legality = Legality { standard :: Bool
--                          , modern :: Bool
--                          , legacy :: Bool
--                          , vintage :: Bool
--                          } deriving (Eq)

-- $(makeLenses ''Legality)

-- 110.5
data Status =
  Status { _tapped  :: Bool
         , _flipped :: Bool
         , _faceUp  :: Bool
         , _phased  :: Bool
         , _sick    :: Bool} deriving (Eq, Show)

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
  { _name       :: String
  , _manaCost   :: Maybe [Pip]
  , _color      :: [Color]
  , _identity   :: [Color]
  , _keywords   :: [Keyword]
  , _typeLine   :: TypeLine
  , _oracleText :: String
  , _power      :: Maybe PT
  , _toughness  :: Maybe PT
  , _loyalty    :: Maybe Int
  -- , _function   :: Game ()
  } deriving (Eq)

$(makeLenses ''Properties)

instance Show Properties where
  show p = show (p^.name) ++ " " ++ mc (p^.manaCost) ++ "\n" ++
           tl (p^.typeLine) ++
           tb (p^.oracleText) ++
           f (p^.power, p^.toughness)
   where f (Just x, Just y) = show x ++ "/" ++ show y ++ "\n"
         f _                = ""
         g []     = ""
         g (x:xs) = "{" ++ show x ++ "}" ++ g xs
         tl (TypeLine [] [] []) = ""
         tl typeline            = show typeline ++ "\n"
         tb textbox = if null textbox then "" else textbox ++ "\n"
         mc Nothing  = ""
         mc (Just p) = g p


-- 109.1
-- Token | Copy are represented by Nothing being passed to Permanent and Spell respectively
data ObjectType = Ability | Card | Spell (Maybe GameObject) | Permanent (Maybe GameObject) Status | Emblem deriving (Eq)

data GameObject =
  GameObject { _properties :: Properties
             , _objType    :: ObjectType
             , _owner      :: PId
             , _controller :: PId} deriving (Eq)

$(makeLenses ''GameObject)

instance Ord GameObject where
  a <= b = a ^. (properties . name) <= b ^. (properties . name)

instance Show GameObject where
  show o = show $ o^.properties

isCard o = o^.objType == Card

isSpell o = case o^.objType of
  Spell _ -> True
  _       -> False

copySpell :: GameObject -> GameObject
copySpell s = assert (isSpell s) $ objType .~ Spell Nothing $ s

-- defaultLegality = Legality { standard = True
--                            , modern   = True
--                            , legacy   = True
--                            , vintage  = True
--                            }


defaultProperties :: Properties
defaultProperties = Properties
  { _name = "DefaultCard"
  , _manaCost = Nothing
  , _color = []
  , _identity = []
  , _keywords = []
  , _typeLine = TypeLine [] [] []
  , _oracleText = ""
  , _power = Nothing
  , _toughness = Nothing
  , _loyalty = Nothing
  -- , _legality = defaultLegality
  }

defaultCard =
  GameObject { _properties = defaultProperties
             , _objType = Card
             , _owner = You
             , _controller = You}

-- 202.3
pipValue :: Pip -> Int
pipValue x = case x of
              HyPip a b -> if v a > v b then v a else v b
              p         -> v p
           where v XSym       = 0
                 v (GenSym i) = i
                 v _          = 1

manaValue :: GameObject -> Int
manaValue o = case o^.properties.manaCost of
  Just pips -> foldr ((+) . pipValue) 0 pips

isMonoColored :: GameObject -> Bool
isMonoColored o = length (o^.properties.color) == 1

isMultiColored :: GameObject -> Bool
isMultiColored o = length (o^.properties.color) > 1

isColorless :: GameObject -> Bool
isColorless o = null (o^.properties.color)

