module SleightofMind where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


sleightofMind = (properties.name .~ "Sleight of Mind") . (properties.manaCost ?~ [CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Instant] []) . (properties.oracleText .~ "Change the text of target spell or permanent by replacing all instances of one color word with another. (For example, you may change \"target black spell\" to \"target blue spell.\" This effect lasts indefinitely.)") $ defaultCard
