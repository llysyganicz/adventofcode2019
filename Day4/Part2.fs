module Part2

open System

let getData() = 
  [ 353096 .. 843212 ]

let isIncreasing n =
  let sorted = n.ToString() |> Seq.sort |> String.Concat
  n = (int sorted)

let isRepeated n =
  let text = n.ToString()
  text |> Seq.mapi (fun i e -> if i < text.Length - 1 then e = text.[i + 1] else false)
       |> Seq.exists id

let oneDouble n =
  n.ToString() |> Seq.groupBy id |> Seq.exists (fun (k, v) -> v |> Seq.length = 2)

let getResult() =
  let result = getData() |> List.filter isIncreasing |> List.filter isRepeated |> List.filter oneDouble
  result.Length