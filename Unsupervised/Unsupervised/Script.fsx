
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

//---------- Basic dataset statistics
printfn "%16s %8s %8s %8s" "Tag name" "Avg" "Min" "Max"

headers
|> Array.iteri (fun i name ->
    let col = observations |> Array.map (fun obs -> obs.[i])
    let avg = col |> Array.average
    let min = col |> Array.min
    let max = col |> Array.max
    printfn "%16s %8.1f %8.1f %8.1f" name avg min max)

//---------- Add charting referernces
#I @"..\packages\"
#r @"FSharp.Charting.0.91.1\lib\net45\FSharp.Charting.dll"
open FSharp.Charting
open System.Drawing

let labels = ChartTypes.LabelStyle(Interval = 0.25)
headers
|> Seq.mapi (fun i name ->
    name,
    observations
    |> Seq.averageBy (fun obs -> obs.[i]))
|> Chart.Bar
|> Chart.WithTitle (Text = "Average Usage by Tag", FontStyle = FontStyle.Bold, Color = Color.Red, FontSize = 20.)
|> Chart.WithXAxis (LabelStyle = labels)
|> Chart.Show
