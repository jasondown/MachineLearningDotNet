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
let survived (p : Passenger) = p.Survived
