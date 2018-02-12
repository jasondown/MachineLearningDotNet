#I @"..\packages"
#r @"FSharp.Data.2.4.4\lib\net45\FSharp.Data.dll"
#load "Tree.fs"

open FSharp.Data
open Titanic
open Titanic.Tree
open System.Drawing

type Titanic = CsvProvider<"titanic.csv">
type Passenger = Titanic.Row 

let dataset = Titanic.GetSample()

let label (p : Passenger) = p.Survived

let features = [
    "Sex", fun (p : Passenger) -> p.Sex |> Some
    "Class", fun p -> p.Pclass |> string |> Some
    "Age", fun p -> if p.Age < 7.0 then Some "Younger" else Some "Older" ]

let tree = growTree dataset.Rows label (features |> Map.ofList)

dataset.Rows
|> Seq.averageBy (fun p -> if p.Survived = decide tree p then 1. else 0.)

display 0 tree

growTree2 [ entropyGainFilter; leafSizeFilter 10 ] dataset.Rows label (features |> Map.ofList)
|> display 0

//---------- K-Fold (Generating k training/validation pairs to prevent over-fitting
let kfold k sample =
    let size = sample |> Array.length
    let foldSize = size / k

    [ for f in 0 .. (k-1) do
        let sliceStart = f * foldSize
        let sliceEnd = f * foldSize + foldSize - 1
        let validation = sample.[sliceStart .. sliceEnd]
        let training =
            [|
                for i in 0 .. (sliceStart - 1) do yield sample.[i]
                for i in (sliceEnd + 1) .. (size - 1) do yield sample.[i]
            |]
        yield training, validation
    ]