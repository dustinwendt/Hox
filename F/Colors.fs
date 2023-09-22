module Colors

// open Enum

type Color = | White | Blue | Black | Red | Green
type Mana = | Colored of Color | Colorless
type Pip = | CSym of Mana | XSym | PhySym | SnowSym | GenSym of int | HyPip of Pip * Pip
type ManaPool = Map<Mana, int>

let colorList = [White; Blue; Black; Red; Green]
// let colorSeq = Enum.GetValues(typeof<Color>) |> Seq.cast<Color>

let pools : Mana list = [Colorless] @ List.map (fun c -> Colored c) colorList


let emptyManaPool : ManaPool = Map.ofList (List.map (fun x -> (x, 0)) pools)


let pipValue x =
    let v (y: Pip) =
        match y with
            | XSym -> 0
            | GenSym i -> i
            | _        -> 1
    match x with
        | HyPip (a,b) -> if v a > v b then v a else v b
        | p            -> v p

