module Types

open Colors

// generic type
// 'a list  === list<'a>

type PId = | You | Opp
type SId = String
type Id  = String

// Haskell
// data Stack a = EmptyStack | StackNode a

// F#
// type 'a stack =
//     | EmptyStack
//     | StackNode of 'a * 'a stack

type Attack = Id * PId
type Defend = Id * Id
type Combat =
    { attackers : Attack list
      defenders : Defend list}

type PT =
     | Star
     | StarPlus of int
     | PT of int

type Pip =
  | Undefined


type Phase =
    | Untap
    | Upkeep
    | Draw
    | Main
    | BegCom
    | DecAttack of Combat
    | DecBlock of Combat
    | FirstDamCom of Combat
    | DamCom of Combat
    | EndCom
    | End
    | Cleanup

type Keyword =
    | Banding
    | Defender
    | FirstStrike
    | Fearmod
    | Flying
    | Haste
    | Indestructible
    | LandWalk
    | Protection
    | Reach
    | Regeneration
    | Trample
    | Vigilance

type Properties =
    { name : string
      manaCost : Pip list option
      color : Color list
      identity: Color list
      keywords: Keyword list
      typeLine: string
      oracleText: string
      power : PT option
      toughness : PT option
      loyalty: int option

            }


// [] :: 'a list

type Status =
    { tapped: bool
      flipped: bool
      faceUp: bool
      phased: bool
      sick: bool}

let noCombat :Combat = { attackers = ([] : Attack list)
                         defenders = ([] : Defend list) }

type GameObject =
    { properties: Properties
      owner: PId
      controller: PId }

let defaultProperties =
    { name = "DefaultCard"
      manaCost = None
      color = []
      identity = []
      keywords = []
      typeLine = ""
      oracleText = ""
      power = None
      toughness = None
      loyalty = None }

// not allowed in Haskell
// type Foo = { a : int }
// type Bar = { a : int}

let defaultGameObject x =
    { properties = defaultProperties
      owner = x
      controller = x}

type Library = GameObject list
type Hand = GameObject list
type Battlefield = GameObject list
type Graveyard = GameObject list
type Stack = GameObject list
type Exile = GameObject list
type Command = GameObject list

type Player =
    { life : int
      library: Library
      hand: Hand
      graveyard: Graveyard
      // phaseActions: Map<Phase, effectful thing somehow >
      manapool: ManaPool
      maxHandSize: int
      landsPlayed: int
      maxLand: int }

// let test = emptyManaPool

let foo: ManaPool = Map.ofList [(Colorless, 0); (Colored White, 0); (Colored Black, 0)]

let defaultPlayer x =
    { life = 20
      library = List.replicate 60 (defaultGameObject x)
      hand = []
      graveyard = []
      manapool = emptyManaPool
      maxHandSize = 7
      landsPlayed = 0
      maxLand = 1 }

// GameState -> (a, GameState)
// let test = {defaultPlayer with manapool = emptyManaPool }

type InfiniteType = { test: InfiniteType }


type GameState =
    { players: Map<PId, Player>
      phaseActions: Map<PId, Map<Phase, (GameState -> GameState) list>>
      stack: Stack
      activePlayer: PId
      exile: Exile
      battlefield: Battlefield
      currPhase: Phase
      phases: Phase list
      turnOrder: PId list
      turns: PId list
      priority: PId option
      stormCount: int
      precombat: bool
      passes: int
      ids: Id list
      test: string}

// Need to get this into GameState somehow
// type PhaseActions = Map<Phase, (GameState -> GameState) list>

// Map<PId <Map<Phase, (GameState ->GameState ) list>>


// cycle test: can't do cycles
// let rec cycle x = x @ cycle x

let defaultGameState =
    { players = Map.ofList [(You, (defaultPlayer You)); (Opp, (defaultPlayer Opp))]
      phaseActions = Map.empty
      stack   = []
      activePlayer = You
      exile   = []
      battlefield = []
      currPhase = Untap
      phases = [Untap]
      turnOrder = [You; Opp]
      turns = [You; Opp]
      stormCount = 0
      priority = None
      precombat = true
      passes = 0
      ids = []
      test = "Hello from GameState!"
    }




