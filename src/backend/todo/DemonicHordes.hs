module DemonicHordes where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


demonicHordes = (properties.name .~ "Demonic Hordes") . (properties.manaCost ?~ [GenSym 3,CSym (Colored Black),CSym (Colored Black),CSym (Colored Black)]) . (properties.color .~ [Black]) . (properties.identity .~ [Black]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Creature] [CType Demon]) . (properties.oracleText .~ "{T}: Destroy target land.\nAt the beginning of your upkeep, unless you pay {B}{B}{B}, tap Demonic Hordes and sacrifice a land of an opponent's choice.") . (properties.power .~ (Just (PT 5))) . (properties.toughness .~ (Just (PT 5))) $ defaultCard
