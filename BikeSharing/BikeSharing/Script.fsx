#I @"..\packages\"
#r @"FSharp.Data.2.4.4\lib\net45\FSharp.Data.dll"
#r @"FSharp.Charting.0.91.1\lib\net45\FSharp.Charting.dll"

open FSharp.Data
open FSharp.Charting

type Data = CsvProvider<"day.csv">
let dataset = Data.GetSample()
let data = dataset.Rows

//---------- Show all bikes
let all = 
    Chart.Line [ for observation in data -> observation.Cnt ]
    |> Chart.Show

//---------- Plot original bike counts with moving averages over 7 days and 30 days
let count = data |> Seq.map (fun row -> float row.Cnt)

let movingAvg n (series : float seq) =
    series
    |> Seq.windowed n
    |> Seq.map (fun xs -> xs |> Seq.average)
    |> Seq.toList

Chart.Combine [
    Chart.Line count
    Chart.Line (movingAvg 7 count)
    Chart.Line (movingAvg 30 count) ]
    |> Chart.Show
