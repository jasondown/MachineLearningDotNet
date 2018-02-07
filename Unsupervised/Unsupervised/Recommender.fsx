open System
open System.IO

let folder = __SOURCE_DIRECTORY__
let file = "userprofiles-toptags.txt"

let headers, observations =
    let raw = Path.Combine(folder, file) |> File.ReadAllLines

    let headers = (raw.[0].Split ',').[1..] // skip user id column

    let observations =
        raw.[1..]
        |> Array.map (fun line -> (line.Split ',').[1..])
        |> Array.map (Array.map float)
        |> Array.filter (fun row -> (row |> Array.sum) > 0.)

    headers, observations

//----------Prepare the dataset
let scale (row : float []) =
    let min = row |> Array.min
    let max = row |> Array.max
    if min = max then row
    else row |> Array.map (fun x -> (x - min) / (max - min))

let test = observations.[..99] |> Array.map scale
let train = observations.[100..] |> Array.map scale

//----------Helper functions
let distance (row1 : float []) (row2 : float []) =
    (row1, row2)
    ||> Array.map2 (fun x y -> pown (x - y) 2)
    |> Array.sum

let similarity (row1 : float []) (row2 : float []) =
    1. / (1. + distance row1 row2)

let split (row : float []) =
    row.[..19], row.[20..]

let weights (values : float []) =
    let total = values |> Array.sum
    values
    |> Array.map (fun x -> x / total)

//----------Computing tag predictions for a user
let predict (row : float []) =
    let known, unknown = row |> split
    let similarities =
        train
        |> Array.map (fun example ->
            let common, _ = example |> split
            similarity known common)
        |> weights
    [| for i in 20 .. 29 ->
        let column = train |> Array.map (fun x -> x.[i])
        let prediction =
            (similarities, column)
            ||> Array.map2 (fun s v -> s * v)
            |> Array.sum
        prediction |]

//----------Test it out!
let targetTags = headers.[20..]
predict test.[0] |> Array.zip targetTags
test.[0] |> split |> snd

//---------- Percentage correct recommendations
let validation =
    test
    |> Array.map (fun obs ->
        let actual = obs |> split |> snd
        let predicted = obs |> predict
        let recommended, observed =
            Array.zip predicted actual
            |> Array.maxBy fst
        if observed > 0. then 1. else 0.)
    |> Array.average
    |> printfn "Correct calls: %f"

//----------Naive recommendation accuracy
let averages = [|
    for i in 20 .. 29 ->
        train |> Array.averageBy (fun row -> row.[i]) |]

let baseline =
    test
    |> Array.map (fun obs ->
        let actual = obs |> split |> snd
        let predicted = averages
        let recommended, observed =
            Array.zip predicted actual
            |> Array.maxBy fst
        if observed > 0. then 1. else 0.)
    |> Array.average
    |> printfn "Correct calls (Baseline): %f"