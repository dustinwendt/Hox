module CardList( strToCard,module ShivanDragon,module WillotheWisp,module OrcishOriflamme,module ProdigalSorcerer,module BlueElementalBlast,module IcyManipulator,module GlassesofUrza,module StealArtifact,module Fungusaur,module DarkRitual,module Lance,module WildGrowth,module WallofStone,module NorthernPaladin,module DrainLife,module JayemdaeTome,module RaiseDead,module ZombieMaster,module MonssGoblinRaiders,module ShanodinDryads,module LightningBolt,module CrawWurm,module PsychicVenom,module PowerSink,module Swamp,module PsionicBlast,module SwordstoPlowshares,module Sacrifice,module RedElementalBlast,module ConsecrateLand,module ReverseDamage,module VerduranEnchantress,module CircleofProtectionWhite,module ElvishArchers,module CircleofProtectionRed,module Terror,module Burrowing,module Mountain,module FalseOrders,module SirensCall,module TimeVault,module HypnoticSpecter,module ManaFlare,module WoodenSphere,module PlagueRats,module KormusBell,module UnholyStrength,module TimeWalk,module Unsummon,module ScrybSprites,module Lich,module StoneGiant,module Regeneration,module DisruptingScepter,module LivingWall,module Fastbond,module Badlands,module Invisibility,module WallofBone,module CreatureBond,module Lifetap,module Web,module GuardianAngel,module WallofWood,module SamiteHealer,module Jump,module BlueWard,module WallofIce,module WhiteWard,module RocofKherRidges,module Balance,module SerraAngel,module WallofBrambles,module CopperTablet,module TimberWolves,module CursedLand,module MindTwist,module BlackKnight,module PersonalIncarnation,module StreamofLife,module CircleofProtectionBlue,module BirdsofParadise,module ThicketBasilisk,module Tunnel,module HowlingMine,module ClockworkBeast,module NaturalSelection,module MerfolkofthePearlTrident,module BenalishHero,module AnimateWall,module IllusionaryMask,module Gloom,module PhantomMonster,module WarMammoth,module FireElemental,module BlazeofGlory,module Paralyze,module MoxJet,module GreenWard,module WheelofFortune,module RodofRuin,module WhiteKnight,module Stasis,module Fireball,module BogWraith,module DwarvenWarriors,module RagingRiver,module MesaPegasus,module Clone,module Sinkhole,module WrathofGod,module VolcanicEruption,module Counterspell,module Manabarbs,module Tundra,module ChaosOrb,module Pestilence,module IvoryCup,module AncestralRecall,module RoyalAssassin,module MahamotiDjinn,module Berserk,module Karma,module Smoke,module SoulNet,module SedgeTroll,module Conservator,module Flight,module AspectofWolf,module GoblinBalloonBrigade,module JadeMonolith,module EvilPresence,module Fork,module Fear,module PearledUnicorn,module GrizzlyBears,module BadMoon,module WallofAir,module Camouflage,module PhantasmalForces,module Flashfires,module Firebreathing,module Plains,module Tsunami,module CircleofProtectionGreen,module AirElemental,module LivingArtifact,module IslandSanctuary,module Purelace,module ObsianusGolem,module Timetwister,module GiantGrowth,module StoneRain,module Thoughtlace,module Weakness,module NettlingImp,module UthdenTroll,module Righteousness,module SavannahLions,module WordofCommand,module TropicalIsland,module Channel,module DrainPower,module Taiga,module GaeasLiege,module JadeStatue,module InstillEnergy,module Resurrection,module Lifeforce,module WinterOrb,module Farmstead,module AnimateDead,module Bayou,module HillGiant,module Armageddon,module BasaltMonolith,module WallofWater,module CopyArtifact,module ScavengingGhoul,module Plateau,module Disenchant,module ControlMagic,module IronStar,module GoblinKing,module IceStorm,module Forest,module LeyDruid,module DwarvenDemolitionTeam,module Braingeyser,module Fog,module HolyStrength,module UndergroundSea,module PirateShip,module WarpArtifact,module DragonWhelp,module LordofAtlantis,module DemonicHordes,module Castle,module Disintegrate,module Cockatrice,module Twiddle,module IronclawOrcs,module VeteranBodyguard,module MagicalHack,module Earthquake,module WallofSwords,module MoxSapphire,module NevinyrralsDisk,module BlackVise,module TwoHeadedGiantofForiys,module SengirVampire,module ManaVault,module KeldonWarlord,module HurloonMinotaur,module SolRing,module Shatter,module DemonicAttorney,module PhantasmalTerrain,module NetherShadow,module Simulacrum,module Wanderlust,module Savannah,module HowlfromBeyond,module WaterElemental,module BlackLotus,module ThroneofBone,module PowerLeak,module ContractfromBelow,module Deathlace,module Tranquility,module CelestialPrism,module HealingSalve,module GiantSpider,module AnimateArtifact,module Kudzu,module GraniteGargoyle,module Conversion,module CrystalRod,module Lifelace,module VesuvanDoppelganger,module Lure,module FrozenShade,module HolyArmor,module SleightofMind,module HelmofChatzuk,module Meekstone,module EarthElemental,module Feedback,module RedWard,module DemonicTutor,module IronrootTreefolk,module Juggernaut,module AnkhofMishra,module LivingLands,module SeaSerpent,module GrayOgre,module TheHive,module BlackWard,module Regrowth,module SunglassesofUrza,module MoxEmerald,module SpellBlast,module MoxRuby,module MoxPearl,module LlanowarElves,module WallofFire,module Blessing,module Chaoslace,module ScatheZombies,module DingusEgg,module Hurricane,module DrudgeSkeletons,module GauntletofMight,module LordofthePit,module RockHydra,module Darkpact,module CyclopeanTomb,module OrcishArtillery,module Scrubland,module PowerSurge,module ManaShort,module Earthbind,module Nightmare,module Island,module Deathgrip,module LibraryofLeng,module Forcefield,module DeathWard,module ForceofNature) where

import           AirElemental
import           AncestralRecall
import           AnimateArtifact
import           AnimateDead
import           AnimateWall
import           AnkhofMishra
import           Armageddon
import           AspectofWolf
import           Badlands
import           BadMoon
import           Balance
import           BasaltMonolith
import           Bayou
import           BenalishHero
import           Berserk
import           BirdsofParadise
import           BlackKnight
import           BlackLotus
import           BlackVise
import           BlackWard
import           BlazeofGlory
import           Blessing
import           BlueElementalBlast
import           BlueWard
import           BogWraith
import           Braingeyser
import           Burrowing
import           Camouflage
import           Card
import           Castle
import           CelestialPrism
import           Channel
import           Chaoslace
import           ChaosOrb
import           CircleofProtectionBlue
import           CircleofProtectionGreen
import           CircleofProtectionRed
import           CircleofProtectionWhite
import           ClockworkBeast
import           Clone
import           Cockatrice
import           ConsecrateLand
import           Conservator
import           ContractfromBelow
import           ControlMagic
import           Conversion
import           CopperTablet
import           CopyArtifact
import           Counterspell
import           CrawWurm
import           CreatureBond
import           CrystalRod
import           CursedLand
import           CyclopeanTomb
import           Darkpact
import           DarkRitual
import           Deathgrip
import           Deathlace
import           DeathWard
import           DemonicAttorney
import           DemonicHordes
import           DemonicTutor
import           DingusEgg
import           Disenchant
import           Disintegrate
import           DisruptingScepter
import           DragonWhelp
import           DrainLife
import           DrainPower
import           DrudgeSkeletons
import           DwarvenDemolitionTeam
import           DwarvenWarriors
import           Earthbind
import           EarthElemental
import           Earthquake
import           ElvishArchers
import           EvilPresence
import           FalseOrders
import           Farmstead
import           Fastbond
import           Fear
import           Feedback
import           Fireball
import           Firebreathing
import           FireElemental
import           Flashfires
import           Flight
import           Fog
import           Forcefield
import           ForceofNature
import           Forest
import           Fork
import           FrozenShade
import           Fungusaur
import           GaeasLiege
import           GauntletofMight
import           GiantGrowth
import           GiantSpider
import           GlassesofUrza
import           Gloom
import           GoblinBalloonBrigade
import           GoblinKing
import           GraniteGargoyle
import           GrayOgre
import           GreenWard
import           GrizzlyBears
import           GuardianAngel
import           HealingSalve
import           HelmofChatzuk
import           HillGiant
import           HolyArmor
import           HolyStrength
import           HowlfromBeyond
import           HowlingMine
import           HurloonMinotaur
import           Hurricane
import           HypnoticSpecter
import           IceStorm
import           IcyManipulator
import           IllusionaryMask
import           InstillEnergy
import           Invisibility
import           IronclawOrcs
import           IronrootTreefolk
import           IronStar
import           Island
import           IslandSanctuary
import           IvoryCup
import           JadeMonolith
import           JadeStatue
import           JayemdaeTome
import           Juggernaut
import           Jump
import           Karma
import           KeldonWarlord
import           KormusBell
import           Kudzu
import           Lance
import           LeyDruid
import           LibraryofLeng
import           Lich
import           Lifeforce
import           Lifelace
import           Lifetap
import           LightningBolt
import           LivingArtifact
import           LivingLands
import           LivingWall
import           LlanowarElves
import           LordofAtlantis
import           LordofthePit
import           Lure
import           MagicalHack
import           MahamotiDjinn
import           Manabarbs
import           ManaFlare
import           ManaShort
import           ManaVault
import           Meekstone
import           MerfolkofthePearlTrident
import           MesaPegasus
import           MindTwist
import           MonssGoblinRaiders
import           Mountain
import           MoxEmerald
import           MoxJet
import           MoxPearl
import           MoxRuby
import           MoxSapphire
import           NaturalSelection
import           NetherShadow
import           NettlingImp
import           NevinyrralsDisk
import           Nightmare
import           NorthernPaladin
import           ObsianusGolem
import           OrcishArtillery
import           OrcishOriflamme
import           Paralyze
import           PearledUnicorn
import           PersonalIncarnation
import           Pestilence
import           PhantasmalForces
import           PhantasmalTerrain
import           PhantomMonster
import           PirateShip
import           PlagueRats
import           Plains
import           Plateau
import           PowerLeak
import           PowerSink
import           PowerSurge
import           ProdigalSorcerer
import           PsionicBlast
import           PsychicVenom
import           Purelace
import           RagingRiver
import           RaiseDead
import           RedElementalBlast
import           RedWard
import           Regeneration
import           Regrowth
import           Resurrection
import           ReverseDamage
import           Righteousness
import           RockHydra
import           RocofKherRidges
import           RodofRuin
import           RoyalAssassin
import           Sacrifice
import           SamiteHealer
import           Savannah
import           SavannahLions
import           ScatheZombies
import           ScavengingGhoul
import           Scrubland
import           ScrybSprites
import           SeaSerpent
import           SedgeTroll
import           SengirVampire
import           SerraAngel
import           ShanodinDryads
import           Shatter
import           ShivanDragon
import           Simulacrum
import           Sinkhole
import           SirensCall
import           SleightofMind
import           Smoke
import           SolRing
import           SoulNet
import           SpellBlast
import           Stasis
import           StealArtifact
import           StoneGiant
import           StoneRain
import           StreamofLife
import           SunglassesofUrza
import           Swamp
import           SwordstoPlowshares
import           Taiga
import           Terror
import           TheHive
import           ThicketBasilisk
import           Thoughtlace
import           ThroneofBone
import           TimberWolves
import           Timetwister
import           TimeVault
import           TimeWalk
import           Tranquility
import           TropicalIsland
import           Tsunami
import           Tundra
import           Tunnel
import           Twiddle
import           TwoHeadedGiantofForiys
import           UndergroundSea
import           UnholyStrength
import           Unsummon
import           UthdenTroll
import           VerduranEnchantress
import           VesuvanDoppelganger
import           VeteranBodyguard
import           VolcanicEruption
import           WallofAir
import           WallofBone
import           WallofBrambles
import           WallofFire
import           WallofIce
import           WallofStone
import           WallofSwords
import           WallofWater
import           WallofWood
import           Wanderlust
import           WarMammoth
import           WarpArtifact
import           WaterElemental
import           Weakness
import           Web
import           WheelofFortune
import           WhiteKnight
import           WhiteWard
import           WildGrowth
import           WillotheWisp
import           WinterOrb
import           WoodenSphere
import           WordofCommand
import           WrathofGod
import           ZombieMaster

strToCard :: String -> GameObject
strToCard "shivanDragon"             = shivanDragon
strToCard "willotheWisp"             = willotheWisp
strToCard "orcishOriflamme"          = orcishOriflamme
strToCard "prodigalSorcerer"         = prodigalSorcerer
strToCard "blueElementalBlast"       = blueElementalBlast
strToCard "icyManipulator"           = icyManipulator
strToCard "glassesofUrza"            = glassesofUrza
strToCard "stealArtifact"            = stealArtifact
strToCard "fungusaur"                = fungusaur
strToCard "darkRitual"               = darkRitual
strToCard "lance"                    = lance
strToCard "wildGrowth"               = wildGrowth
strToCard "wallofStone"              = wallofStone
strToCard "northernPaladin"          = northernPaladin
strToCard "drainLife"                = drainLife
strToCard "jayemdaeTome"             = jayemdaeTome
strToCard "raiseDead"                = raiseDead
strToCard "zombieMaster"             = zombieMaster
strToCard "monssGoblinRaiders"       = monssGoblinRaiders
strToCard "shanodinDryads"           = shanodinDryads
strToCard "lightningBolt"            = lightningBolt
strToCard "crawWurm"                 = crawWurm
strToCard "psychicVenom"             = psychicVenom
strToCard "powerSink"                = powerSink
strToCard "swamp"                    = swamp
strToCard "psionicBlast"             = psionicBlast
strToCard "swordstoPlowshares"       = swordstoPlowshares
strToCard "sacrifice"                = sacrifice
strToCard "redElementalBlast"        = redElementalBlast
strToCard "consecrateLand"           = consecrateLand
strToCard "reverseDamage"            = reverseDamage
strToCard "verduranEnchantress"      = verduranEnchantress
strToCard "circleofProtectionWhite"  = circleofProtectionWhite
strToCard "elvishArchers"            = elvishArchers
strToCard "circleofProtectionRed"    = circleofProtectionRed
strToCard "terror"                   = terror
strToCard "burrowing"                = burrowing
strToCard "mountain"                 = mountain
strToCard "falseOrders"              = falseOrders
strToCard "sirensCall"               = sirensCall
strToCard "timeVault"                = timeVault
strToCard "hypnoticSpecter"          = hypnoticSpecter
strToCard "manaFlare"                = manaFlare
strToCard "woodenSphere"             = woodenSphere
strToCard "plagueRats"               = plagueRats
strToCard "kormusBell"               = kormusBell
strToCard "unholyStrength"           = unholyStrength
strToCard "timeWalk"                 = timeWalk
strToCard "unsummon"                 = unsummon
strToCard "scrybSprites"             = scrybSprites
strToCard "lich"                     = lich
strToCard "stoneGiant"               = stoneGiant
strToCard "regeneration"             = regeneration
strToCard "disruptingScepter"        = disruptingScepter
strToCard "livingWall"               = livingWall
strToCard "fastbond"                 = fastbond
strToCard "badlands"                 = badlands
strToCard "invisibility"             = invisibility
strToCard "wallofBone"               = wallofBone
strToCard "creatureBond"             = creatureBond
strToCard "lifetap"                  = lifetap
strToCard "web"                      = web
strToCard "guardianAngel"            = guardianAngel
strToCard "wallofWood"               = wallofWood
strToCard "samiteHealer"             = samiteHealer
strToCard "jump"                     = jump
strToCard "blueWard"                 = blueWard
strToCard "wallofIce"                = wallofIce
strToCard "whiteWard"                = whiteWard
strToCard "rocofKherRidges"          = rocofKherRidges
strToCard "balance"                  = balance
strToCard "serraAngel"               = serraAngel
strToCard "wallofBrambles"           = wallofBrambles
strToCard "copperTablet"             = copperTablet
strToCard "timberWolves"             = timberWolves
strToCard "cursedLand"               = cursedLand
strToCard "mindTwist"                = mindTwist
strToCard "blackKnight"              = blackKnight
strToCard "personalIncarnation"      = personalIncarnation
strToCard "streamofLife"             = streamofLife
strToCard "circleofProtectionBlue"   = circleofProtectionBlue
strToCard "birdsofParadise"          = birdsofParadise
strToCard "thicketBasilisk"          = thicketBasilisk
strToCard "tunnel"                   = tunnel
strToCard "howlingMine"              = howlingMine
strToCard "clockworkBeast"           = clockworkBeast
strToCard "naturalSelection"         = naturalSelection
strToCard "merfolkofthePearlTrident" = merfolkofthePearlTrident
strToCard "benalishHero"             = benalishHero
strToCard "animateWall"              = animateWall
strToCard "illusionaryMask"          = illusionaryMask
strToCard "gloom"                    = gloom
strToCard "phantomMonster"           = phantomMonster
strToCard "warMammoth"               = warMammoth
strToCard "fireElemental"            = fireElemental
strToCard "blazeofGlory"             = blazeofGlory
strToCard "paralyze"                 = paralyze
strToCard "moxJet"                   = moxJet
strToCard "greenWard"                = greenWard
strToCard "wheelofFortune"           = wheelofFortune
strToCard "rodofRuin"                = rodofRuin
strToCard "whiteKnight"              = whiteKnight
strToCard "stasis"                   = stasis
strToCard "fireball"                 = fireball
strToCard "bogWraith"                = bogWraith
strToCard "dwarvenWarriors"          = dwarvenWarriors
strToCard "ragingRiver"              = ragingRiver
strToCard "mesaPegasus"              = mesaPegasus
strToCard "clone"                    = clone
strToCard "sinkhole"                 = sinkhole
strToCard "wrathofGod"               = wrathofGod
strToCard "volcanicEruption"         = volcanicEruption
strToCard "counterspell"             = counterspell
strToCard "manabarbs"                = manabarbs
strToCard "tundra"                   = tundra
strToCard "chaosOrb"                 = chaosOrb
strToCard "pestilence"               = pestilence
strToCard "ivoryCup"                 = ivoryCup
strToCard "ancestralRecall"          = ancestralRecall
strToCard "royalAssassin"            = royalAssassin
strToCard "mahamotiDjinn"            = mahamotiDjinn
strToCard "berserk"                  = berserk
strToCard "karma"                    = karma
strToCard "smoke"                    = smoke
strToCard "soulNet"                  = soulNet
strToCard "sedgeTroll"               = sedgeTroll
strToCard "conservator"              = conservator
strToCard "flight"                   = flight
strToCard "aspectofWolf"             = aspectofWolf
strToCard "goblinBalloonBrigade"     = goblinBalloonBrigade
strToCard "jadeMonolith"             = jadeMonolith
strToCard "evilPresence"             = evilPresence
strToCard "fork"                     = fork
strToCard "fear"                     = fear
strToCard "pearledUnicorn"           = pearledUnicorn
strToCard "grizzlyBears"             = grizzlyBears
strToCard "badMoon"                  = badMoon
strToCard "wallofAir"                = wallofAir
strToCard "camouflage"               = camouflage
strToCard "phantasmalForces"         = phantasmalForces
strToCard "flashfires"               = flashfires
strToCard "firebreathing"            = firebreathing
strToCard "plains"                   = plains
strToCard "tsunami"                  = tsunami
strToCard "circleofProtectionGreen"  = circleofProtectionGreen
strToCard "airElemental"             = airElemental
strToCard "livingArtifact"           = livingArtifact
strToCard "islandSanctuary"          = islandSanctuary
strToCard "purelace"                 = purelace
strToCard "obsianusGolem"            = obsianusGolem
strToCard "timetwister"              = timetwister
strToCard "giantGrowth"              = giantGrowth
strToCard "stoneRain"                = stoneRain
strToCard "thoughtlace"              = thoughtlace
strToCard "weakness"                 = weakness
strToCard "nettlingImp"              = nettlingImp
strToCard "uthdenTroll"              = uthdenTroll
strToCard "righteousness"            = righteousness
strToCard "savannahLions"            = savannahLions
strToCard "wordofCommand"            = wordofCommand
strToCard "tropicalIsland"           = tropicalIsland
strToCard "channel"                  = channel
strToCard "drainPower"               = drainPower
strToCard "taiga"                    = taiga
strToCard "gaeasLiege"               = gaeasLiege
strToCard "jadeStatue"               = jadeStatue
strToCard "instillEnergy"            = instillEnergy
strToCard "resurrection"             = resurrection
strToCard "lifeforce"                = lifeforce
strToCard "winterOrb"                = winterOrb
strToCard "farmstead"                = farmstead
strToCard "animateDead"              = animateDead
strToCard "bayou"                    = bayou
strToCard "hillGiant"                = hillGiant
strToCard "armageddon"               = armageddon
strToCard "basaltMonolith"           = basaltMonolith
strToCard "wallofWater"              = wallofWater
strToCard "copyArtifact"             = copyArtifact
strToCard "scavengingGhoul"          = scavengingGhoul
strToCard "plateau"                  = plateau
strToCard "disenchant"               = disenchant
strToCard "controlMagic"             = controlMagic
strToCard "ironStar"                 = ironStar
strToCard "goblinKing"               = goblinKing
strToCard "iceStorm"                 = iceStorm
strToCard "forest"                   = forest
strToCard "leyDruid"                 = leyDruid
strToCard "dwarvenDemolitionTeam"    = dwarvenDemolitionTeam
strToCard "braingeyser"              = braingeyser
strToCard "fog"                      = fog
strToCard "holyStrength"             = holyStrength
strToCard "undergroundSea"           = undergroundSea
strToCard "pirateShip"               = pirateShip
strToCard "warpArtifact"             = warpArtifact
strToCard "dragonWhelp"              = dragonWhelp
strToCard "lordofAtlantis"           = lordofAtlantis
strToCard "demonicHordes"            = demonicHordes
strToCard "castle"                   = castle
strToCard "disintegrate"             = disintegrate
strToCard "cockatrice"               = cockatrice
strToCard "twiddle"                  = twiddle
strToCard "ironclawOrcs"             = ironclawOrcs
strToCard "veteranBodyguard"         = veteranBodyguard
strToCard "magicalHack"              = magicalHack
strToCard "earthquake"               = earthquake
strToCard "wallofSwords"             = wallofSwords
strToCard "moxSapphire"              = moxSapphire
strToCard "nevinyrralsDisk"          = nevinyrralsDisk
strToCard "blackVise"                = blackVise
strToCard "twoHeadedGiantofForiys"   = twoHeadedGiantofForiys
strToCard "sengirVampire"            = sengirVampire
strToCard "manaVault"                = manaVault
strToCard "keldonWarlord"            = keldonWarlord
strToCard "hurloonMinotaur"          = hurloonMinotaur
strToCard "solRing"                  = solRing
strToCard "shatter"                  = shatter
strToCard "demonicAttorney"          = demonicAttorney
strToCard "phantasmalTerrain"        = phantasmalTerrain
strToCard "netherShadow"             = netherShadow
strToCard "simulacrum"               = simulacrum
strToCard "wanderlust"               = wanderlust
strToCard "savannah"                 = savannah
strToCard "howlfromBeyond"           = howlfromBeyond
strToCard "waterElemental"           = waterElemental
strToCard "blackLotus"               = blackLotus
strToCard "throneofBone"             = throneofBone
strToCard "powerLeak"                = powerLeak
strToCard "contractfromBelow"        = contractfromBelow
strToCard "deathlace"                = deathlace
strToCard "tranquility"              = tranquility
strToCard "celestialPrism"           = celestialPrism
strToCard "healingSalve"             = healingSalve
strToCard "giantSpider"              = giantSpider
strToCard "animateArtifact"          = animateArtifact
strToCard "kudzu"                    = kudzu
strToCard "graniteGargoyle"          = graniteGargoyle
strToCard "conversion"               = conversion
strToCard "crystalRod"               = crystalRod
strToCard "lifelace"                 = lifelace
strToCard "vesuvanDoppelganger"      = vesuvanDoppelganger
strToCard "lure"                     = lure
strToCard "frozenShade"              = frozenShade
strToCard "holyArmor"                = holyArmor
strToCard "sleightofMind"            = sleightofMind
strToCard "helmofChatzuk"            = helmofChatzuk
strToCard "meekstone"                = meekstone
strToCard "earthElemental"           = earthElemental
strToCard "feedback"                 = feedback
strToCard "redWard"                  = redWard
strToCard "demonicTutor"             = demonicTutor
strToCard "ironrootTreefolk"         = ironrootTreefolk
strToCard "juggernaut"               = juggernaut
strToCard "ankhofMishra"             = ankhofMishra
strToCard "livingLands"              = livingLands
strToCard "seaSerpent"               = seaSerpent
strToCard "grayOgre"                 = grayOgre
strToCard "theHive"                  = theHive
strToCard "blackWard"                = blackWard
strToCard "regrowth"                 = regrowth
strToCard "sunglassesofUrza"         = sunglassesofUrza
strToCard "moxEmerald"               = moxEmerald
strToCard "spellBlast"               = spellBlast
strToCard "moxRuby"                  = moxRuby
strToCard "moxPearl"                 = moxPearl
strToCard "llanowarElves"            = llanowarElves
strToCard "wallofFire"               = wallofFire
strToCard "blessing"                 = blessing
strToCard "chaoslace"                = chaoslace
strToCard "scatheZombies"            = scatheZombies
strToCard "dingusEgg"                = dingusEgg
strToCard "hurricane"                = hurricane
strToCard "drudgeSkeletons"          = drudgeSkeletons
strToCard "gauntletofMight"          = gauntletofMight
strToCard "lordofthePit"             = lordofthePit
strToCard "rockHydra"                = rockHydra
strToCard "darkpact"                 = darkpact
strToCard "cyclopeanTomb"            = cyclopeanTomb
strToCard "orcishArtillery"          = orcishArtillery
strToCard "scrubland"                = scrubland
strToCard "powerSurge"               = powerSurge
strToCard "manaShort"                = manaShort
strToCard "earthbind"                = earthbind
strToCard "nightmare"                = nightmare
strToCard "island"                   = island
strToCard "deathgrip"                = deathgrip
strToCard "libraryofLeng"            = libraryofLeng
strToCard "forcefield"               = forcefield
strToCard "deathWard"                = deathWard
strToCard "forceofNature"            = forceofNature
strToCard _                          = defaultCard
