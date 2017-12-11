open System;
let toIntArray (input: string) = input.Split [|'\t';' ';'\r'|] |> Array.map Int32.Parse

let redistribute (bank: int []) = 
    let rec redisHelper (left: int) (index: int) (bank: int[]) = 
        bank.[index] <- bank.[index] + 1
        let left = left - 1;
        match left with
        | 0 -> bank
        | l -> 
            match index + 1 with
            | x when x = bank.Length -> redisHelper l 0 bank
            | i -> redisHelper l i bank

    let startIndex = bank |> Array.mapi (fun i v -> i, v) |> Array.maxBy snd |> fst
    let toDistribute = bank.[startIndex]
    bank.[startIndex] <- 0
    match startIndex + 1 with
    | x when x = bank.Length -> redisHelper toDistribute 0 bank
    | i -> redisHelper toDistribute i bank

let rec checker (cycle: int) (seenPatterns: int [][]) (bank: int []) =
    match Array.contains bank seenPatterns with 
    | true -> cycle - (Array.findIndex (fun x -> x = bank) seenPatterns) + 1
    | false -> 
        let seenPatterns = Array.append seenPatterns [|Array.copy bank|] 
        let bank = redistribute bank
        checker (cycle+1) seenPatterns bank

let main (input: string) = 
    input 
    |> toIntArray
    |> checker 0 [|[||]|]



let test = "2	8	8	5	4	2	3	1	5	5	1	2	15	13	5	14"

