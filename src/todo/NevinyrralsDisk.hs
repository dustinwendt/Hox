module NevinyrralsDisk where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


nevinyrralsDisk = (properties.name .~ "Nevinyrral's Disk") . (properties.manaCost ?~ [GenSym 4]) . (properties.color .~ []) . (properties.identity .~ []) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Artifact] []) . (properties.oracleText .~ "Nevinyrral's Disk enters the battlefield tapped.
{1}, {T}: Destroy all artifacts, creatures, and enchantments.") $ defaultCard
