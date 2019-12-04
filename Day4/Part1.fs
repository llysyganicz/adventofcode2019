module Part1

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

let getResult() =
  let result = getData() |> List.filter isIncreasing |> List.filter isRepeated
  result.Length