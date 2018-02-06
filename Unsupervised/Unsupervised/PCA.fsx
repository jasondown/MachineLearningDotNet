#I @"..\packages"
#r @"MathNet.Numerics.3.20.2\lib\net40\MathNet.Numerics.dll"
#r @"MathNet.Numerics.FSharp.3.20.2\lib\net40\MathNet.Numerics.FSharp.dll"
#r @"FSharp.Charting.0.91.1\lib\net45\FSharp.Charting.dll"

open MathNet
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Statistics
open FSharp.Charting
open FSharp.Charting.ChartTypes

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

let correlations = 
    observations
    |> Matrix.Build.DenseOfColumnArrays
    |> Matrix.toRowArrays
    |> Correlation.PearsonMatrix

let features = headers.Length
let correlated = 
    [
        for col in 0 .. (features - 1) do
            for row in (col + 1) .. (features - 1) ->
                correlations.[col, row], headers.[col], headers.[row]
    ]   
    |> Seq.sortByDescending (fun (corr, f1, f2) -> corr)
    |> Seq.take 20
    |> Seq.iter (fun (corr, f1, f2) ->
        printfn "%s %s : %.2f" f1 f2 corr)

//----------Applying PCA to StackOverflow dataset
#load "PCA.fs"
open Unsupervised.PCA
open MathNet.Numerics.Statistics.Statistics
open FSharp.Charting.ChartTypes

let normalized = normalize (headers.Length) observations
let (eValues, eVectors), projector = pca normalized

//----------Feature weight analysis
let total = eValues |> Seq.sumBy (fun x -> x.Magnitude)
eValues
|> Vector.toList
|> List.rev
|> List.scan (fun (percent, acc) value ->
    let percent = 100. * value.Magnitude / total
    let acc = acc + percent
    (percent, acc)) (0., 0.)
|> List.tail
|> List.iteri (fun i (p, c) -> printfn "Feature: %2i: %.2f%% (%.2f%%)" i p c)

// Plotting original features against extracted components
let principalComponent comp1 comp2 =
    let title = sprintf "Component %i vs %i" comp1 comp2
    let features = headers.Length
    let coords = Seq.zip (eVectors.Column(features-comp1)) (eVectors.Column(features-comp2))
    Chart.Point (coords, Title = title, Labels = headers, MarkerSize = 7)
    |> Chart.WithXAxis (
        Min = -1.0, 
        Max = 1.0, 
        LabelStyle = ChartTypes.LabelStyle (Interval = 0.25))
    |> Chart.WithYAxis (
        Min = -1.0, 
        Max = 1.0, 
        LabelStyle = ChartTypes.LabelStyle (Interval = 0.25))
    |> Chart.Show

//---------- Plotting observations against the principal components
let projections comp1 comp2 =
    let title = sprintf "Component %i vs %i" comp1 comp2
    let features = headers.Length
    let coords =
        normalized
        |> Seq.map projector
        |> Seq.map (fun obs -> obs.[features-comp1], obs.[features-comp2])
    Chart.Point (coords, Title = title)
    |> Chart.WithXAxis (
        Min = -200., 
        Max = 500., 
        LabelStyle = ChartTypes.LabelStyle (Interval = 100.))
    |> Chart.WithYAxis (
        Min = -200., 
        Max = 500., 
        LabelStyle = ChartTypes.LabelStyle (Interval = 100.))
    |> Chart.Show
    