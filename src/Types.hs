module Types where

-- 205.2 Card Types
-- 205.2a
data CardType = Artifact | Conspiracy | Creature | Dungeon
              | Enchantment | Instant | Land | Phenomenon
              | Plane | Planeswalker  | Scheme | Sorcery | Tribal | Vanguard deriving (Eq)

-- 205.3 Subtypes
-- 205.3g
data ArtifactType = Clue | Contraption | Equipment | Food | Fortification | Gold | Treasure | Vehicle deriving (Eq)

-- 205.3h
data EnchantmentType = Aura | Cartouche | Class | Curse | Rune | Saga | Shard | Shrine deriving (Eq)

-- 205.3i
data LandType = Desert | Forest | Gate | Island | Lair | Locus | Mine | Mountain | Plains
              | PowerPlant | Swamp | Tower | Urzas deriving (Eq)

-- 205.3j
data PlaneswalkerType = Ajani | Aminatou | Angrath | Arlinn | Ashiok | Bahamut | Basri
  | Bolas | Calix | Chandra | Dack | Dakkon | Daretti | Davriel | Dihada | Domri | Dovin | Ellywick
  | Elspeth | Estrid | Freyalise | Garruk | Gideon | Grist | Huatli | Jace | Jaya | Jeska | Karn | Kasmina
  | Kaya | Kiora | Koth | Liliana | Lolth | Lukka | Mordenkainen | Nahiri | Narset | Niko | Nissa | Nixilis
  | Oko | Ral | Rowan | Saheeli | Samut | Sarkhan | Serra | Sorin | Szat | Tamiyo | Teferi | Teyo | Tezzeret
  | Tibalt | Tyvar | Ugin | Venser | Vivien | Vraska | Will | Windgrace | Wrenn | Xenagos | Yanggu | Yanling | Zariel deriving (Eq)

-- 205.3k
data SpellType = Adventure | Arcane | Lesson | Trap deriving (Eq)

-- 205.3l
data CreatureType = Advisor | Aetherborn | Ally | Angel | Antelope | Ape | Archer | Archon | Army | Artificer | Assassin | AssemblyWorker | Atog | Aurochs | Avatar | Azra | Badger | Barbarian | Bard | Basilisk | Bat | Bear | Beast
  | Beeble | Beholder | Berserker | Bird | Blinkmoth | Boar | Bringer | Brushwagg | Camarid | Camel | Caribou | Carrier | Cat | Centaur | Cephalid | Chimera | Citizen | Cleric | Cockatrice | Construct | Coward | Crab | Crocodile
  | Cyclops | Dauthi | Demigod | Demon | Deserter | Devil | Dinosaur | Djinn | Dog | Dragon | Drake | Dreadnought | Drone | Druid | Dryad | Dwarf | Efreet | Egg | Elder | Eldrazi | Elemental | Elephant | Elf | Elk | Eye | Faerie
  | Ferret | Fish | Flagbearer | Fox | Fractal | Frog | Fungus | Gargoyle | Germ | Giant | Gnoll | Gnome | Goat | Goblin | God | Golem | Gorgon | Graveborn | Gremlin | Griffin | Hag | Halfling | Hamster | Harpy | Hellion | Hippo
  | Hippogriff | Homarid | Homunculus | Horror | Horse | Human | Hydra | Hyena | Illusion | Imp | Incarnation | Inkling | Insect | Jackal | Jellyfish | Juggernaut | Kavu | Kirin | Kithkin | Knight | Kobold | Kor | Kraken | Lamia
  | Lammasu | Leech | Leviathan | Lhurgoyf | Licid | Lizard | Manticore | Masticore | Mercenary | Merfolk | Metathran | Minion | Minotaur | Mole | Monger | Mongoose | Monk | Monkey | Moonfolk | Mouse | Mutant | Myr | Mystic | Naga
  | Nautilus | Nephilim | Nightmare | Nightstalker | Ninja | Noble | Noggle | Nomad | Nymph | Octopus | Ogre | Ooze | Orb | Orc | Orgg | Otter | Ouphe | Ox | Oyster | Pangolin | Peasant | Pegasus | Pentavite | Pest | Phelddagrif
  | Phoenix | Phyrexian | Pilot | Pincher | Pirate | Plant | Praetor | Prism | Processor | Rabbit | Ranger | Rat | Rebel | Reflection | Rhino | Rigger | Rogue | Sable | Salamander | Samurai | Sand | Saproling | Satyr | Scarecrow
  | Scion | Scorpion | Scout | Sculpture | Serf | Serpent | Servo | Shade | Shaman | Shapeshifter | Shark | Sheep | Siren | Skeleton | Slith | Sliver | Slug | Snake | Soldier | Soltari | Spawn | Specter | Spellshaper | Sphinx
  | Spider | Spike | Spirit | Splinter | Sponge | Squid | Squirrel | Starfish | Surrakar | Survivor | Tentacle | Tetravite | Thalakos | Thopter | Thrull | Tiefling | Treefolk | Trilobite | Triskelavite | Troll | Turtle | Unicorn
  | Vampire  | Vedalken | Viashino | Volver | Wall | Warlock | Warrior | Weird | Werewolf | Whale | Wizard | Wolf | Wolverine | Wombat | Worm | Wraith | Wurm | Yeti | Zombie | Zubera deriving (Eq)

-- 205.3m
data PlaneType = Alara | Arkhos | Azgol | Belenon | BolasMeditationRealm | Dominaria | Equilor | Ergamon | Fabacin | Innistrad | Iquatana | Ir | Kaldheim | Kamigawa | Karsus | Kephalai | Kinshala | Kolbahan | Kyneth
  | Lorwyn | Luvion | Mercadia | Mirrodin | Moag | Mongseng | Muraganda | NewPhyrexia | Phyrexia | Pyrulea | Rabiah | Rath | Ravnica | Regatha | Segovia | SerrasRealm | Shadowmoor | Shandalar | Ulgrotha | Valla | Vryn
  | Wildfire | Xerex | Zendikar deriving (Eq)

-- 205.4a
data SuperType = Basic | Legendary | Ongoing | Snow | World deriving (Eq)

-- 205.3m
type TribalType = CreatureType

data SubType = AType ArtifactType | EType EnchantmentType | LType LandType | WType PlaneswalkerType | SType SpellType | CType CreatureType | PType PlaneType deriving (Eq)

data TypeLine = TypeLine [SuperType] [CardType] [SubType] deriving (Eq)
