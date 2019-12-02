module Part1

  open System.IO

  let getData() =
    let input = File.ReadAllText("input.txt")
    input.Split ',' |> Seq.map (fun x -> int x) |> Seq.toList

  let replaceStartingValues i v =
    match i with
    | 1 -> 12
    | 2 -> 2
    | _ -> v

  let replaceValue ic ir vc vr =
    match (ic, ir) with
    | (x, y) when x = y -> vr
    | _ -> vc

  let add (input : int list) pos1 pos2 pos3 =
    let result = input.[pos1] + input.[pos2]
    input |> List.mapi (fun i e ->  replaceValue i pos3 e result)

  let multiply (input : int list) pos1 pos2 pos3 = 
    let result = input.[pos1] * input.[pos2]
    input |> List.mapi (fun i e ->  replaceValue i pos3 e result)

  let rec runProgram (position : int) (input : int list) : int list = 
    match input.[position] with
    | 1 -> runProgram (position + 4) (add input input.[position + 1] input.[position + 2] input.[position + 3])
    | 2 -> runProgram (position + 4) (multiply input input.[position + 1] input.[position + 2] input.[position + 3])
    | _ -> input

  let getResult() =
    getData()
    |> List.mapi replaceStartingValues
    |> runProgram 0 |> List.head