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