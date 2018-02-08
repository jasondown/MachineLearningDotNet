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

let byClass =
    dataset.Rows
    |> Seq.groupBy (fun p -> p.Pclass)

byClass
|> Seq.iter (fun (s, g) ->
    printfn "Class %A: %f" s (survivalRate g))

//----------Decision Stump
let mostFrequentLabelIn group =
    group
    |> Seq.countBy snd
    |> Seq.maxBy snd
    |> fst

let learn sample extractFeature extractLabel =
    let groups =
        sample
        |> Seq.map (fun obs -> extractFeature obs, extractLabel obs)
        |> Seq.groupBy fst
        |> Seq.map (fun (feat, group) -> feat, mostFrequentLabelIn group)
    let classifier obs =
        let featureValue = extractFeature obs
        groups 
        |> Seq.find (fun (f, _) -> f = featureValue)
        |> snd
    classifier

//----------Train and evaluate the stump
let survived (p : Passenger) = p.Survived
let sex (p : Passenger) = p.Sex
let sexClassifier = survived |> learn (dataset.Rows) sex

printfn "Stump: Classify based on passenger sex."
dataset.Rows
|> Seq.averageBy (fun p -> 
    if p.Survived = sexClassifier p then 1.0 else 0.0)

//----------Train and evaluate stump with multiple prongs
let classClassifier = survived |> learn (dataset.Rows) (fun p -> p.Pclass)

printfn "Stump: Classifiy based on passenger class (3 pronged)"
dataset.Rows
|> Seq.averageBy (fun p -> 
    if p.Survived = classClassifier p then 1.0 else 0.0)