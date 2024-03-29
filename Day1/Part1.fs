module Part1

open System.IO

let getData() =
    let lines = File.ReadLines("input.txt")
    lines |> Seq.map (fun x -> int x)

let getResult() =
    getData()
    |> Seq.map (fun x -> x / 3 - 2)
    |> Seq.sum