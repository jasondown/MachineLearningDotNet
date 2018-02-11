#I @"..\packages"
#r @"FSharp.Data.2.4.4\lib\net45\FSharp.Data.dll"
#load "Tree.fs"

open System
open FSharp.Data
open Titanic
open Titanic.Tree

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

let rec display depth tree =
    let padding = String.replicate (2 * depth) " "
    match tree with
    | Answer label -> printfn " -> %A" label
    | Stump ((name, _), _, branches) ->
        printfn ""
        branches
        |> Seq.iter (fun kv ->
            printf "%s ? %s : %s" padding name kv.Key
            display (depth + 1) kv.Value)