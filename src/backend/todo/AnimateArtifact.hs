module AnimateArtifact where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


animateArtifact = (properties.name .~ "Animate Artifact") . (properties.manaCost ?~ [GenSym 3,CSym (Colored Blue)]) . (properties.color .~ [Blue]) . (properties.identity .~ [Blue]) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Enchantment] [EType Aura]) . (properties.oracleText .~ "Enchant artifact\nAs long as enchanted artifact isn't a creature, it's an artifact creature with power and toughness each equal to its mana value.") $ defaultCard
