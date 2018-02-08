#I @"..\packages"
#r @"FSharp.Data.2.4.4\lib\net45\FSharp.Data.dll"

open FSharp.Data

type Titanic = CsvProvider<"titanic.csv">
type Passenger = Titanic.Row 

let dataset = Titanic.GetSample()

//----------Establishing a baseline
dataset.Rows
|> Seq.countBy (fun passenger -> passenger.Survived)
|> Seq.iter (printfn "%A")

dataset.Rows
|> Seq.averageBy (fun passenger ->
    if passenger.Survived then 1.0 else 0.0)
|> printfn "Chances of survival: %.3f"

//----------Exploring survival rates for different groups.
let survivalRate (passengers : Passenger seq) = 
    let total = passengers |> Seq.length
    let survivors =
        passengers
        |> Seq.filter (fun p -> p.Survived)
        |> Seq.length
    100. * (float survivors / float total)

let bySex =
    dataset.Rows
    |> Seq.groupBy (fun p -> p.Sex)

bySex
|> Seq.iter (fun (s, g) ->
    printfn "Sex %A: %f" s (survivalRate g))

