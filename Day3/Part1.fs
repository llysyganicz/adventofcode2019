module Part1

  open System.IO

  type Point = { X: int; Y: int }

  type Move =
  | L of int
  | U of int
  | R of int
  | D of int

  let parseMove (move: string) =
    let direction = move.[0]
    let length = int (move.Substring 1)
    match direction with
    | 'L' -> L length
    | 'U' -> U length
    | 'R' -> R length
    | _ -> D length

  let parseLine (line: string) =
    line.Split ',' |> Seq.map parseMove |> Seq.toList

  let getData() =
    let lines = File.ReadLines("input.txt") |> Seq.toList
    (parseLine lines.[0], parseLine lines.[1])
  
  let rec getPoints (moves: Move list) x y (points: Point list) =
    match moves with
    | m::t -> let newPoints = match m with
                              | L l -> [ x .. -1 .. x - l ] |> List.map(fun e -> { X = e; Y = y })
                              | U l -> [ y .. y + l ] |> List.map(fun e -> { X = x; Y = e })
                              | R l -> [ x .. x + l ] |> List.map(fun e -> { X = e; Y = y })
                              | D l -> [ y .. -1 .. y - l ] |> List.map(fun e -> { X = x; Y = e })
              let lastPoint = newPoints |> List.rev |> List.head
              getPoints t lastPoint.X  lastPoint.Y points @ newPoints
    | [] -> points |> List.distinct

  let getResult() =
    let (wire1, wire2) = getData()
    let points1 = getPoints wire1 0 0 []
    let points2 = getPoints wire2 0 0 []
    let crosses = Set.intersect (Set.ofList points1) (Set.ofList points2)
    crosses |> Set.map (fun e -> abs e.X +  abs e.Y ) |> Set.remove 0 |> Set.minElement
    