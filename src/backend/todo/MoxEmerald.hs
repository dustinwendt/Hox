module MoxEmerald where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


moxEmerald = (properties.name .~ "Mox Emerald") . (properties.manaCost ?~ [GenSym 0]) . (properties.color .~ []) . (properties.identity .~ [Green]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Artifact] []) . (properties.oracleText .~ "{T}: Add {G}.") $ defaultCard
