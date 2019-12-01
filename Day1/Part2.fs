module Part2

open System.IO

let getData() =
    let lines = File.ReadLines("input.txt")
    lines |> Seq.map (fun x -> int x)

let rec getFuel x sum =
    let value = x / 3 - 2
    if value > 0 then
        getFuel value (sum + value)
    else
        sum

let getResult() =
    getData()
    |> Seq.map (fun x -> getFuel x 0)
    |> Seq.sum