module TimeVault where

import Colors
import ComplexTypes
import Control.Lens
import Data.Maybe
import Types


timeVault = (properties.name .~ "Time Vault") . (properties.manaCost ?~ [GenSym 2]) . (properties.color .~ []) . (properties.identity .~ []) . (properties.keywords .~ []) . (properties.typeLine .~ TypeLine [] [Artifact] []) . (properties.oracleText .~ "Time Vault enters the battlefield tapped.\nTime Vault doesn't untap during your untap step.\nIf you would begin your turn while Time Vault is tapped, you may skip that turn instead. If you do, untap Time Vault.\n{T}: Take an extra turn after this one.") $ defaultCard
