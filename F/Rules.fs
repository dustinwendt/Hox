module Rules

open Types

let rec take n xs =
    match n, xs with
        | 0, _ -> []
        | _, [] -> []
        | o, (h::t) -> h :: (take (o-1) t)

let rec drop n (xs: list<'a>) =
    match n, xs with
        | 0, xs -> xs
        | _, [] -> []
        | o, h::t -> drop (o-1) t


// drawN :: n -> Player -> Player
let drawN (n:int) (p:Player):Player =
    { p with hand = p.hand @ (take n p.library); library = (drop n p.library)}


// Map.change :: (key: 'Key) (f: T' option -> T' option) (table : Map<'Key, 'T>) -> Map <'Key, 'T>

// Option.map :: 'a -> 'b -> 'a option -> 'b option

// Says that the result is an int?
// GameState -> PId -> int -> GameState

// (a -> (b option -> b option) -> Map<a,b> )
let draw (g:GameState) (pid:PId) (n:int):GameState =
    let (u:Player) = Map.find pid g.players
    let (x:Player) = drawN n u// Player
    {g with players = Map.add pid x g.players }
    // { g with players = Map.change pid (Option.map (drawN n)) g.players }
    // { g with players = Map.change pid (drawN (Map.find pid g.players) n) (Map.find pid g.players)}


// let nextPhase :: p =

let pass (g: GameState) : GameState =
    match g.currPhase with
        | Untap -> { g with priority = (Some g.activePlayer); currPhase = Upkeep}
        | Upkeep -> { g with currPhase = Draw}
        | Draw   -> { g with currPhase = Main}
        | Main -> if g.precombat then { g with precombat = false; currPhase = BegCom} else {g with currPhase = End}
        | BegCom -> {g with currPhase = (DecAttack noCombat)}
        | DecAttack x -> {g with currPhase = (DecBlock x)}
        | DecBlock x -> {g with currPhase = (FirstDamCom x) }
        | FirstDamCom x -> {g with currPhase = (DamCom x)}
        | DamCom x -> {g with currPhase = EndCom}
        | EndCom -> {g with currPhase = Main}
        | End -> {g with currPhase = Cleanup; priority = None}
        | Cleanup -> {g with currPhase = Untap; priority = None}





