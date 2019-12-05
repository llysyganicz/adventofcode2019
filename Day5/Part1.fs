module Part1

  open System.IO

  let mutable input = 0
  let mutable output = 0

  let getData() =
    let input = File.ReadAllText("input.txt")
    input.Split ',' |> Seq.map (fun x -> int x) |> Seq.toList

  let replaceValue ic ir vc vr =
    match (ic, ir) with
    | (x, y) when x = y -> vr
    | _ -> vc

  let add (instructions : int list) val1 val2 pos =
    instructions |> List.mapi (fun i e ->  replaceValue i pos e (val1 + val2))

  let multiply (instructions : int list) val1 val2 pos = 
    instructions |> List.mapi (fun i e ->  replaceValue i pos e (val1 * val2))

  let write (instructions : int list) pos =
    instructions |> List.mapi(fun i e -> replaceValue i pos e input)

  let read (instructions : int list) pos =
    output <- instructions.[pos]
    instructions

  let getValues (instructions: int list) position =
    let val1 = match (instructions.[position] / 100) % 10 with
               | 1 -> instructions.[position + 1]
               | _ -> instructions.[instructions.[position + 1]]
    let val2 = match (instructions.[position] / 1000) % 10 with
               | 1 -> instructions.[position + 2]
               | _ -> instructions.[instructions.[position + 2]]
    (val1, val2)

  let rec runProgram (position : int) (instructions : int list) : int list =
    let opcode = instructions.[position] % 10
    match opcode with
    | 1 ->
      let values = getValues instructions position
      runProgram (position + 4) (add instructions (fst values) (snd values) instructions.[position + 3])
    | 2 -> 
      let values = getValues instructions position
      runProgram (position + 4) (multiply instructions (fst values) (snd values) instructions.[position + 3])
    | 3 -> 
      runProgram (position + 2) (write instructions instructions.[position + 1])
    | 4 -> 
      runProgram (position + 2) (read instructions instructions.[position + 1])
    | _ -> instructions

  let getResult() =
    input <- 1
    getData() |> runProgram 0 |> ignore
    output