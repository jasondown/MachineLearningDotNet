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

//----------Feature with continuous numeric values (fare)
let survivalByFarePrice =
    dataset.Rows 
    |> Seq.groupBy (fun p -> p.Fare)
    |> Seq.iter (fun (price, passengers) ->
        printfn "%6.2F: %6.2f" price (survivalRate passengers))

//----------Discretization of fare prices
let averageFare =
    dataset.Rows
    |> Seq.averageBy (fun p -> p.Fare)

let fareLevel (p : Passenger) =
    if p.Fare < averageFare
    then "Cheap"
    else "Expensive"
let fareClassifier = survived |> learn (dataset.Rows) fareLevel

printfn "Stump: Classify based on fare level."
dataset.Rows
|> Seq.averageBy (fun p -> 
    if p.Survived = fareClassifier p then 1.0 else 0.0)

//----------Exploring features with missing data. Port of origin.
let survivalByPortOfOrigin =
    dataset.Rows
    |> Seq.groupBy (fun p -> p.Embarked)
    |> Seq.iter (fun (port, passengers) ->
        printfn "%s: %f" port (survivalRate passengers))

//----------Incorporating missing values
let hasData extractFeature = extractFeature >> Option.isSome

let betterLearn sample extractFeature extractLabel =
    let branches =
        sample
        |> Seq.filter (extractFeature |> hasData)
        |> Seq.map (fun obs -> extractFeature obs |> Option.get, extractLabel obs)
        |> Seq.groupBy fst
        |> Seq.map (fun (feat, group) -> feat, mostFrequentLabelIn group)
        |> Map.ofSeq
    let labelFormMissingValues =
        sample
        |> Seq.countBy extractLabel
        |> Seq.maxBy snd
        |> fst
    let classifier obs = 
        let featureValue = extractFeature obs
        match featureValue with
        | None -> labelFormMissingValues
        | Some (value) ->
            match (branches.TryFind value) with
            | None -> labelFormMissingValues
            | Some (predictedLabel) -> predictedLabel
    classifier

let port (p : Passenger) =
    match p.Embarked with
    | "" -> None
    | e -> Some e

let updatedClassifier = survived |> betterLearn (dataset.Rows) port

printfn "Stump: Classify based on port of origin (optional value)."
dataset.Rows
|> Seq.averageBy (fun p -> 
    if p.Survived = updatedClassifier p then 1.0 else 0.0)

//----------Shannon Entropy
let entropy data =
    let size = data |> Seq.length
    data
    |> Seq.countBy id
    |> Seq.map (fun (_, count) -> float count / float size)
    |> Seq.sumBy (fun f -> if f > 0. then - f * log f else 0.)