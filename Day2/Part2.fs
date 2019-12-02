module Part2

  open System.IO

  let getData() =
    let input = File.ReadAllText("input.txt")
    input.Split ',' |> Seq.map (fun x -> int x) |> Seq.toList

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

  let rec checkInput data noun verb =
    let output = data |> List.mapi (fun i e -> replaceValue i 1 e noun)
                      |> List.mapi (fun i e -> replaceValue i 2 e verb)
                      |> runProgram 0
                      |> List.head

    if output = 19690720 then 
      100 * noun + verb
    else
      match (noun, verb) with
      | n, v when n < 99 -> checkInput data (n + 1) v
      | n, v -> checkInput data 0 (v + 1)

  let getResult() =
    let data = getData()
    checkInput data 0 0