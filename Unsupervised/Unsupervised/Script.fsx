
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

//----------Using K Means Clustering
#load "KMeans.fs"
open Unsupervised.KMeans
open FSharp.Charting.ChartTypes

type Observation = float []
let features = headers.Length

let distance (obs1 : Observation) (obs2 : Observation) =
    (obs1, obs2)
    ||> Seq.map2 (fun u1 u2 -> pown (u1 - u2) 2)
    |> Seq.sum
    |> sqrt

let centroidOf (cluster : Observation seq) =
    Array.init features (fun f -> cluster |> Seq.averageBy (fun user -> user.[f]))

//---------- Clusterizing the dataset using 5 as arbitrary value of k
let observations1 =
    observations
    |> Array.map (Array.map float)
    |> Array.filter (fun x -> Array.sum x > 0.)

let clusters1, classifier1 =
    let clustering = clusterize distance centroidOf
    let k = 5
    clustering observations1 k

clusters1
|> Seq.iter (fun (id, profile) ->
    printfn "CLUSTER %i" id
    profile
    |> Array.iteri (fun i value -> printfn "%16s %.1f" headers.[i] value))

Chart.Combine [
    for (id, profile) in clusters1 ->
        profile
        |> Seq.mapi (fun i value -> headers.[i], value)
        |> Chart.Bar
]   |> Chart.WithXAxis (LabelStyle = labels)
    |> Chart.Show

observations1
|> Seq.countBy (fun obs -> classifier1 obs)
|> Seq.iter (fun (clusterID, count) ->
    printfn "Cluster: %i: %i elements" clusterID count)

//----------Normalizing observations to similar activity
let rowNormalizer (obs : Observation) : Observation =
    let max = obs |> Seq.max
    obs |> Array.map (fun tagUse -> tagUse / max)

let observations2 =
    observations
    |> Array.filter (fun x -> Array.sum x > 0.)
    |> Array.map (Array.map float)
    |> Array.map rowNormalizer

let (clusters2, classifier2) =
    let clustering = clusterize distance centroidOf
    let k = 5
    clustering observations2 k

observations2
|> Seq.countBy (fun obs -> classifier2 obs)
|> Seq.iter (fun (clusterID, count) ->
    printfn "Cluster: %i: %i elements" clusterID count)

Chart.Combine [
    for (id, profile) in clusters2 ->
        profile
        |> Seq.mapi (fun i value -> headers.[i], value)
        |> Chart.Column
]   |> Chart.WithXAxis (LabelStyle = labels)
    |> Chart.Show