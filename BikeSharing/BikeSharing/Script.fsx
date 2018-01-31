#I @"..\packages\"
#r @"FSharp.Data.2.4.4\lib\net45\FSharp.Data.dll"
#r @"FSharp.Charting.0.91.1\lib\net45\FSharp.Charting.dll"

open FSharp.Data
open FSharp.Charting

type Data = CsvProvider<"day.csv">
let dataset = Data.GetSample()
let data = dataset.Rows

let all = 
    Chart.Line [ for observation in data -> observation.Cnt ]
    |> Chart.Show