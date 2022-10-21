module BenalishHero where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


benalishHero = (properties.name .~ "Benalish Hero") . (properties.manaCost ?~ [CSym (Colored White)]) . (properties.color .~ [White]) . (properties.identity .~ [White]) . (properties.keywords .~ [Banding]) . (properties.typeLine .~ TypeLine [] [Creature] [CType Human,CType Soldier]) . (properties.oracleText .~ "Banding (Any creatures with banding, and up to one without, can attack in a band. Bands are blocked as a group. If any creatures with banding you control are blocking or being blocked by a creature, you divide that creature's combat damage, not its controller, among any of the creatures it's being blocked by or is blocking.)") . (properties.power .~ (Just (PT 1))) . (properties.toughness .~ (Just (PT 1))) $ defaultCard
