module Main where

import Control.Monad
import Data.Maybe
import Lib

data Color = White | Blue | Black | Red | Green
data ManaColor = Colored Color | Colorless
data Pip = ManaColor | PhyrexianPip ManaColor | SnowPip | Split Pip Pip | Generic Int

-- 205.2 Card Types
-- 205.2a
data CardType = Artifact | Conspiracy | Creature | Dungeon
              | Enchantment | Instant | Land | Phenomenon
              | Plane | Planeswalker  | Scheme | Sorcery | Tribal | Vanguard
-- 205.3 Subtypes
-- 205.3g
data ArtifactType = Clue | Contraption | Equipment | Food | Fortification | Gold | Treasure | Vehicle

-- 205.3h
data EnchantmentType = Aura | Cartouche | Class | Curse | Rune | Saga | Shard | Shrine

-- 205.3i
data LandType = Desert | Forest | Gate | Island | Lair | Locus | Mine | Mountain | Plains
              | PowerPlant | Swamp | Tower | Urzas

-- 205.3j
data PlaneswalkerType = Ajani | Aminatou | Angrath | Arlinn | Ashiok | Bahamut | Basri
                       | Bolas | Calix | Chandra | Dack | Dakkon | Daretti | Davriel | Dihada | Domri | Dovin | Ellywick
                       | Elspeth | Estrid | Freyalise | Garruk | Gideon | Grist | Huatli | Jace | Jaya | Jeska | Karn | Kasmina
                       | Kaya | Kiora | Koth | Liliana | Lolth | Lukka | Mordenkainen | Nahiri | Narset | Niko | Nissa | Nixilis
                       | Oko | Ral | Rowan | Saheeli | Samut | Sarkhan | Serra | Sorin | Szat | Tamiyo | Teferi | Teyo | Tezzeret
                       | Tibalt | Tyvar | Ugin | Venser | Vivien | Vraska | Will | Windgrace | Wrenn | Xenagos | Yanggu | Yanling | Zariel

-- 205.3k
data SpellType = Adventure | Arcane | Lesson | Trap

-- 205.3l
data CreatureType = Advisor | Aetherborn | Ally | Angel | Antelope | Ape | Archer | Archon | Army | Artificer | Assassin | AssemblyWorker | Atog | Aurochs | Avatar | Azra | Badger | Barbarian | Bard | Basilisk | Bat | Bear | Beast | Beeble | Beholder | Berserker | Bird | Blinkmoth | Boar | Bringer | Brushwagg | Camarid | Camel | Caribou | Carrier | Cat | Centaur | Cephalid | Chimera | Citizen | Cleric | Cockatrice | Construct | Coward | Crab | Crocodile | Cyclops | Dauthi | Demigod | Demon | Deserter | Devil | Dinosaur | Djinn | Dog | Dragon | Drake | Dreadnought | Drone | Druid | Dryad | Dwarf | Efreet | Egg | Elder | Eldrazi | Elemental | Elephant | Elf | Elk | Eye | Faerie | Ferret | Fish | Flagbearer | Fox | Fractal | Frog | Fungus | Gargoyle | Germ | Giant | Gnoll | Gnome | Goat | Goblin | God | Golem | Gorgon | Graveborn | Gremlin | Griffin | Hag | Halfling | Hamster | Harpy | Hellion | Hippo | Hippogriff | Homarid | Homunculus | Horror | Horse | Human | Hydra | Hyena | Illusion | Imp | Incarnation | Inkling | Insect | Jackal | Jellyfish | Juggernaut | Kavu | Kirin | Kithkin | Knight | Kobold | Kor | Kraken | Lamia | Lammasu | Leech | Leviathan | Lhurgoyf | Licid | Lizard | Manticore | Masticore | Mercenary | Merfolk | Metathran | Minion | Minotaur | Mole | Monger | Mongoose | Monk | Monkey | Moonfolk | Mouse | Mutant | Myr | Mystic | Naga | Nautilus | Nephilim | Nightmare | Nightstalker | Ninja | Noble | Noggle | Nomad | Nymph | Octopus | Ogre | Ooze | Orb | Orc | Orgg | Otter | Ouphe | Ox | Oyster | Pangolin | Peasant | Pegasus | Pentavite | Pest | Phelddagrif | Phoenix | Phyrexian | Pilot | Pincher | Pirate | Plant | Praetor | Prism | Processor | Rabbit | Ranger | Rat | Rebel | Reflection | Rhino | Rigger | Rogue | Sable | Salamander | Samurai | Sand | Saproling | Satyr | Scarecrow | Scion | Scorpion | Scout | Sculpture | Serf | Serpent | Servo | Shade | Shaman | Shapeshifter | Shark | Sheep | Siren | Skeleton | Slith | Sliver | Slug | Snake | Soldier | Soltari | Spawn | Specter | Spellshaper | Sphinx | Spider | Spike | Spirit | Splinter | Sponge | Squid | Squirrel | Starfish | Surrakar | Survivor | Tentacle | Tetravite | Thalakos | Thopter | Thrull | Tiefling | Treefolk | Trilobite | Triskelavite | Troll | Turtle | Unicorn | Vampire | Vedalken | Viashino | Volver | Wall | Warlock | Warrior | Weird | Werewolf | Whale | Wizard | Wolf | Wolverine | Wombat | Worm | Wraith | Wurm | Yeti | Zombie | Zubera

-- 205.3m
data PlaneType = Alara | Arkhos | Azgol | Belenon | BolasMeditationRealm | Dominaria | Equilor | Ergamon | Fabacin | Innistrad | Iquatana | Ir | Kaldheim | Kamigawa | Karsus | Kephalai | Kinshala | Kolbahan | Kyneth | Lorwyn | Luvion | Mercadia | Mirrodin | Moag | Mongseng | Muraganda | NewPhyrexia | Phyrexia | Pyrulea | Rabiah | Rath | Ravnica | Regatha | Segovia | SerrasRealm | Shadowmoor | Shandalar | Ulgrotha | Valla | Vryn | Wildfire | Xerex | Zendikar

-- 205.4a
data SuperType = Basic | Legendary | Ongoing | Snow | World

-- 208 Power/Toughness
data StarNum = Star | StarPlus Int
data Power = PowStar StarNum | PowNum Int
data Toughness = TouStar StarNum | TouNum Int

-- 205.3m
type TribalType = CreatureType

isMonoColored :: Card -> Bool
isMonoColored c = case c of
  Card {color = x} -> length x == 1

isMultiColored :: Card -> Bool
isMultiColored c = case c of
  Card {color = x} -> length x > 1

isColorless :: Card -> Bool
isColorless c = case c of
  Card {color = x} -> null x

data SubType = AType ArtifactType | EType EnchantmentType | LType LandType | WType PlaneswalkerType | SType SpellType | CType CreatureType | PType PlaneType

data TypeLine = TypeLine [SuperType] [CardType] [SubType]

data Card = Card { cardName :: String
                 , manaCost :: Maybe Int
                 , color :: [Color]
                 , typeLine :: TypeLine
                 , textBox :: String
                 , power :: Maybe Power
                 , toughness :: Maybe Toughness
                 , loyalty :: Maybe Int
                 , handMod :: Maybe Int
                 , lifeMod :: Maybe Int}

defaultCard :: Card
defaultCard = Card { cardName = "DefaultCard"
                   , manaCost = Nothing
                   , color = []
                   , typeLine = ""
                   , textBox = ""
                   , power = Nothing
                   , toughness = Nothing
                   , loyalty = Nothing
                   , handMod = Nothing
                   , lifeMod = Nothing
                   }

data GameState = A

data TurnstileState = Locked | Unlocked
  deriving (Eq, Show)

data TurnstileOutput = Thank | Open | Tut
  deriving (Eq, Show)

newtype State s a = State { runState :: s -> (a, s) }

state :: (s -> (a,s)) -> State s a
state = State

instance Functor (State s) where
  fmap = liftM

instance Applicative (State s) where
  pure = return
  (<*>) = ap

instance Monad (State s) where
  return x = state (\s -> (x,s))
  p >>= k = state $ \ s0 ->
    let (x, s1) = runState p s0
    in runState (k x) s1

coin, push :: TurnstileState -> (TurnstileOutput, TurnstileState)
coin _ = (Thank, Unlocked)

coinS, pushS :: State TurnstileState TurnstileOutput
coinS = state coin
pushS = state push

push Unlocked = (Open, Locked)
push Locked = (Tut, Locked)

monday :: TurnstileState -> ([TurnstileOutput], TurnstileState)
monday s0 =
  let (a1, s1) = coin s0
      (a2, s2) = push s1
      (a3, s3) = push s2
      (a4, s4) = coin s3
      (a5, s5) = push s4
   in ([a1, a2, a3, a4, a5], s5)

mondayS :: State TurnstileState [TurnstileOutput]
mondayS = sequence [coinS, pushS, pushS, coinS, pushS]

regularPersonS, distractedPersonS, hastyPersonS :: State TurnstileState [TurnstileOutput]

regularPersonS = sequence [coinS, pushS]

distractedPersonS = sequence [coinS]

hastyPersonS = do a1 <- pushS
                  case a1 of
                    Open -> return [a1]
                    _     -> do as <- sequence [coinS, pushS]
                                return (a1:as)

luckyPairS :: Bool -> State TurnstileState Bool
luckyPairS b = do
  if b then distractedPersonS else regularPersonS
  o <- pushS
  return (o == Open)

regularPerson, distractedPerson, hastyPerson :: TurnstileState -> ([TurnstileOutput], TurnstileState)

regularPerson s0 =
  let (a1, s1) = coin s0
      (a2, s2) = push s1
  in ([a1, a2], s2)

distractedPerson s0 =
  let (a1, s1) = coin s0
  in ([a1], s1)

hastyPerson Unlocked = let (a1, s1) = push Unlocked in
                         ([a1], s1)
hastyPerson Locked   = let (a1, s1) = push Locked
                           (a2, s2) = coin s1
                           (a3, s3) = push s2 in
                         ([a1,a2,a3], s3)

tuesday :: TurnstileState -> ([TurnstileOutput], TurnstileState)
tuesday s0 = let (a, s1) = regularPerson s0
                 (b, s2) = hastyPerson s1
                 (c, s3) = distractedPerson s2
                 (d, s4) = hastyPerson s3
             in (a ++ b ++ c ++ d, s4)

luckyPair :: Bool -> TurnstileState -> (Bool, TurnstileState)
luckyPair True s0 = let (a, s1) = distractedPerson s0
                        (b, s2) = push s1 in
                      (True, s2)
luckyPair False s0 = let (a, s1) = regularPerson s0
                         (b, s2) = push s1 in
                       (False, s2)
  -- let (a1, s1) = push s0
  --     (finalList, finalState) = case s1 of
  --                                 Locked -> let (coinOut, coinState) = coin s1
  --                                               (pushOut, pushState) = push coinState
  --                                           in ([a1, coinOut, pushOut], pushState)
  --                                 Unlocked -> ([a1], s1)
  -- in (final
      -- List, finalState)

-- turn : Monad Magic
-- turn = do
--   upkeep
--   draw
  

main :: IO ()
main = putStrLn "Here"
