module Hurricane where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


hurricane = (properties.name .~ "Hurricane") . (properties.manaCost ?~ [XSym,CSym (Colored Green)]) . (properties.color .~ [Green]) . (properties.identity .~ [Green]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Sorcery] []) . (properties.oracleText .~ "Hurricane deals X damage to each creature with flying and each player.") $ defaultCard
