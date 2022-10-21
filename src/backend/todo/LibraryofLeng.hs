module LibraryofLeng where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


libraryofLeng = (properties.name .~ "Library of Leng") . (properties.manaCost ?~ [GenSym 1]) . (properties.color .~ []) . (properties.identity .~ []) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Artifact] []) . (properties.oracleText .~ "You have no maximum hand size.\nIf an effect causes you to discard a card, discard it, but you may put it on top of your library instead of into your graveyard.") $ defaultCard
