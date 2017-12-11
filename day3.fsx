open System;
let findNextHighestOddSquareRoot(x: int) =
    let rec foo (y: int) (x: int) =
        match x with 
        | n when y*y >= n -> y
        | _ -> foo (y+2) x
    foo 1 x

let squareRootToLayer (x: int) = x/2

let getAisleOffsetsForLayer (layer: int) = 
    [|-1; -3; -5; -7;|]
    |> Array.map (fun x -> x*layer)

let getMin (input: int) (x:int) = abs (input-x)

let plusSquare (squareRoot: int) = (+) (squareRoot*squareRoot)

let main (input: int) =
    let squareRoot = findNextHighestOddSquareRoot input
    let square = squareRoot * squareRoot
    let layer = squareRoot |> squareRootToLayer
    let foo = getMin input
    getAisleOffsetsForLayer layer
    |> Array.map (fun x -> x + square)
    |> Array.minBy foo
    |> foo
    |> (+) layer


let main


