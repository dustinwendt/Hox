module MagicalHack where

import Card
import Colors
import Control.Lens
import Data.Maybe
import Types


magicalHack = (properties.name .~ "Magical Hack") . (properties.manaCost ?~ [CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Instant] []) . (properties.oracleText .~ "Change the text of target spell or permanent by replacing all instances of one basic land type with another. (For example, you may change "swampwalk" to "plainswalk." This effect lasts indefinitely.)") $ defaultCard
