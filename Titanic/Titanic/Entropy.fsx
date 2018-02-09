#I @"..\packages"
#r @"FSharp.Data.2.4.4\lib\net45\FSharp.Data.dll"

open FSharp.Data

type Titanic = CsvProvider<"titanic.csv">
type Passenger = Titanic.Row 

let dataset = Titanic.GetSample()

let hasData feature = feature >> Option.isSome

//----------Shannon Entropy
let entropy label data =
    let size = data |> Seq.length
    data
    |> Seq.countBy label
    |> Seq.map (fun (_, count) -> float count / float size)
    |> Seq.sumBy (fun f -> if f > 0. then - f * log f else 0.)

//---------- Average entropy after feature split
let splitEntropy extractLabel extractFeature data =
    let dataWithValues =
        data
        |> Seq.filter (extractFeature |> hasData)
    let size = dataWithValues |> Seq.length
    dataWithValues
    |> Seq.groupBy extractFeature
    |> Seq.sumBy (fun (_, group) ->
        let groupSize = group |> Seq.length
        let probaGroup = float groupSize / float size
        let groupEntropy = group |> entropy extractLabel
        probaGroup * groupEntropy)

//----------Information Gain for different features
// NOTE: Made the all options for consistency and infoGain function below
let survived (p : Passenger) = Some p.Survived
let sex (p : Passenger)      = Some p.Sex
let pclass (p : Passenger)   = Some p.Pclass

let port (p : Passenger) =
    match p.Embarked with
    | "" -> None
    | e  -> Some e

let age (p : Passenger) =
    match p.Age with
    | a when a < 12.0 -> Some "Younger"
    | _               -> Some "Older"

let h = dataset.Rows |> entropy survived
let infoGain feature = dataset.Rows |> splitEntropy survived feature

printfn "Comparsion: Most informatin feature"
printfn "Base entropy %.3f" h

InformationGain sex     |> printfn " Sex: %.3f"
InformationGain pclass  |> printfn " Class: %.3f"
InformationGain port    |> printfn " Port: %.3f"
InformationGain age     |> printfn " Age: %.3f"

//----------Compare information gain