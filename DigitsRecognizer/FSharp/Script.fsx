﻿open System.IO

type Observation = { Label : string; Pixels : int[] }
type Distance = int[] * int[] -> int

let dataPath =  __SOURCE_DIRECTORY__ + @"..\..\Data"
let trainingPath = dataPath + @"\trainingsample.csv"

let toObservation (csv : string) =
    let columns = csv.Split ','
    let label = columns.[0]
    let pixels = columns.[1..] |> Array.map int
    { Label = label; Pixels = pixels }

let reader path =
    let data = File.ReadAllLines path
    data.[1..] |> Array.map toObservation

let manhattanDistance (pixels1, pixels2) =
    Array.zip pixels1 pixels2
    |> Array.map (fun (x,y) -> abs (x-y))
    |> Array.sum

let euclideanDistance (pixels1, pixels2) =
    Array.zip pixels1 pixels2
    |> Array.map (fun (x,y) -> pown (x-y) 2)
    |> Array.sum

let train (trainingset : Observation []) (dist : Distance) =
    let classify (pixels : int[]) =
        trainingset
        |> Array.minBy (fun x -> dist(x.Pixels, pixels))
        |> fun x -> x.Label
    classify

let trainingData = reader trainingPath
let classifier = train trainingData

let validationPath = dataPath + @"\validationsample.csv"
let validationData = reader validationPath

let evaluate validationSet classifier =
    validationSet
    |> Array.averageBy (fun x -> if classifier x.Pixels = x.Label then 1. else 0.)
    |> fun x -> x*100.
    |> printfn "Correctly classified: %.2f%%"

let manhattanClassifier = classifier manhattanDistance
let euclideanClassifier = classifier euclideanDistance

// Test Manhattan Distance
printfn "Manhattan"
evaluate validationData manhattanClassifier

// Test Euclidean Distance
printfn "Euclidean"
evaluate validationData euclideanClassifier