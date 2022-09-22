{-# LANGUAGE TemplateHaskell #-}
module Player where

import Colors
import Control.Lens
import Zones

data Player = Player { _life :: Int
                     , _library :: Library
                     , _hand :: Hand
                     , _graveyard :: Graveyard
                     , _manaPool :: ManaPool
                     , _maxHandSize :: Int
                     , _landsPlayed :: Int
                     , _maxLand :: Int
                     }

$(makeLenses ''Player)
