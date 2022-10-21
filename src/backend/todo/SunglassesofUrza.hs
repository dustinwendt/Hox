module SunglassesofUrza where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


sunglassesofUrza = (properties.name .~ "Sunglasses of Urza") . (properties.manaCost ?~ [GenSym 3]) . (properties.color .~ []) . (properties.identity .~ []) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Artifact] []) . (properties.oracleText .~ "You may spend white mana as though it were red mana.") $ defaultCard
