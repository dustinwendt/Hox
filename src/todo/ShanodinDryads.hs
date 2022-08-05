module ShanodinDryads where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


shanodinDryads = (properties.name .~ "Shanodin Dryads") . (properties.manaCost ?~ [CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Nymph,CType Dryad]) . (properties.oracleText .~ "Forestwalk (This creature can't be blocked as long as defending player controls a Forest.)") . (properties.power .~ (Just (PT 1))) . (properties.toughness .~ (Just (PT 1))) $ defaultCard
