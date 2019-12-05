module Part2

  open System.IO

  type CompareType =
  | LessThen
  | Equal

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

  let jumpIf test value addr =
    match (test, value) with
    | (true, 0) -> None
    | (true, _) -> Some(addr)
    | (false, 0) -> Some(addr)
    | (false, _) -> None

  let compare (instructions : int list) compareType val1 val2 addr =
    match (compareType, val1, val2) with
    | (LessThen, v1, v2) when v1 < v2 -> instructions |> List.mapi (fun i e ->  replaceValue i addr e 1)
    | (Equal, v1, v2) when v1 = v2 -> instructions |> List.mapi (fun i e ->  replaceValue i addr e 1)
    | _ -> instructions |> List.mapi (fun i e ->  replaceValue i addr e 0)

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
    | 5 ->
      let values = getValues instructions position
      match (jumpIf true (fst values) (snd values)) with
      | Some n -> runProgram n instructions
      | None -> runProgram (position + 3) instructions
    | 6 ->
      let values = getValues instructions position
      match (jumpIf false (fst values) (snd values)) with
      | Some n -> runProgram n instructions
      | None -> runProgram (position + 3) instructions
    | 7 ->
      let values = getValues instructions position
      runProgram (position + 4) (compare instructions LessThen (fst values) (snd values) instructions.[position + 3])
    | 8 ->
      let values = getValues instructions position
      runProgram (position + 4) (compare instructions Equal (fst values) (snd values) instructions.[position + 3])
    | _ -> instructions

  let getResult() =
    input <- 0
    getData() |> runProgram 0 |> ignore
    output