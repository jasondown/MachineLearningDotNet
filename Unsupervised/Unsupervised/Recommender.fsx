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
