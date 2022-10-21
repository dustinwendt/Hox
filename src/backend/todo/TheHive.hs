module TheHive where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


theHive = (properties.name .~ "The Hive") . (properties.manaCost ?~ [GenSym 5]) . (properties.color .~ []) . (properties.identity .~ []) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Artifact] []) . (properties.oracleText .~ "{5}, {T}: Create a 1/1 colorless Insect artifact creature token with flying named Wasp. (It can't be blocked except by creatures with flying or reach.)") $ defaultCard
