module SpellBlast where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


spellBlast = (properties.name .~ "Spell Blast") . (properties.manaCost ?~ [XSym,CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Instant] []) . (properties.oracleText .~ "Counter target spell with mana value X. (For example, if that spell's mana cost is {3}{U}{U}, X is 5.)") $ defaultCard
