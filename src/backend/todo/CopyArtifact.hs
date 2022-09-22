module CopyArtifact where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


copyArtifact = (properties.name .~ "Copy Artifact") . (properties.manaCost ?~ [GenSym 1,CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] []) . (properties.oracleText .~ "You may have Copy Artifact enter the battlefield as a copy of any artifact on the battlefield, except it's an enchantment in addition to its other types.") $ defaultCard
