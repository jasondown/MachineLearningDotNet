#I @"..\packages"
#r @"MathNet.Numerics.3.20.2\lib\net40\MathNet.Numerics.dll"
#r @"MathNet.Numerics.FSharp.3.20.2\lib\net40\MathNet.Numerics.FSharp.dll"

open MathNet
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Statistics

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