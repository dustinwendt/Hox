module WoodenSphere where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


woodenSphere = (properties.name .~ "Wooden Sphere") . (properties.manaCost ?~ [GenSym 1]) . (properties.color .~ []) . (properties.identity .~ []) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Artifact] []) . (properties.oracleText .~ "Whenever a player casts a green spell, you may pay {1}. If you do, you gain 1 life.") $ defaultCard
