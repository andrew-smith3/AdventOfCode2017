open System;

let incIndex (numbers: int[]) (index: int) = (index + 1) % numbers.Length

let decIndex (numbers: int[]) (index: int) =
    match index-1 with
    | x when x < 0 -> numbers.Length - 1
    | x -> x

let reverse (index: int) (length: int) (numbers: int[]) = 
    let rec reverseHelper (startIndex: int) (endIndex: int) (steps: int) (numbers: int []) = 
        match steps = 0 with 
        | false -> 
            let s = numbers.[startIndex]
            let e = numbers.[endIndex]
            numbers.[startIndex] <- e
            numbers.[endIndex] <- s
            let si = incIndex numbers startIndex
            let ei = decIndex numbers endIndex
            reverseHelper si ei (steps-1) numbers
        | true -> numbers

    let steps = length/2
    let endIndex = (index + (length-1)) % numbers.Length
    reverseHelper index endIndex steps numbers

let hash (state: int * int * int[]) (length: int) =
    let (position, skip, numbers) = state;
    let numbers = reverse position length numbers
    let position = (position + length + skip) % numbers.Length
    (position, skip+1, numbers)

let main (input:string) = 
    let initialState = (0, 0, [|0..255|])
    input.Split [|','|]
    |> Array.map Int32.Parse
    |> Array.fold hash initialState
    |> fun (_, _, nums) -> nums
    |> fun nums -> nums.[0] * nums.[1]

//part2

let appendAddition (lengths: int[]) = 
    Array.append lengths [|17; 31; 73; 47; 23|]

let hash64 (lengths: int[]) =
    
    let rec helper (round: int) (state: int * int * int[]) = 
        match round with
        | 64 -> state
        | _ -> 
            lengths 
            |> Array.fold hash state
            |> helper (round+1)

    let initState = (0,0, [|0..255|])
    helper 0 initState

let rec splitBy16 (state: int[] list * int) (numbers: int[]) = 
    let (parts, index) = state
    match index with
    | 256 -> state
    | _ -> 
        let part = Array.sub numbers index 16
        splitBy16 (part::parts, index+16) numbers

let xor (numbers: int[]) = Array.reduce (^^^) numbers

let denseHash (numbers: int[]) = 
    let initState = (List.empty, 0)
    splitBy16 initState numbers
    |> fst
    |> List.map xor

let main2 (input: string) = 
    let initState = (0,0,0, [|0..255|])
    input
    |> Seq.map int
    |> Seq.toArray
    |> appendAddition
    |> hash64
    |> fun (_,_,numbers) -> numbers
    |> denseHash
    |> List.map (fun x -> x.ToString("X2"))
    |> List.rev
    |> String.Concat

let test = "3,4,1,5"

let finalTest = "212,254,178,237,2,0,1,54,167,92,117,125,255,61,159,164"
