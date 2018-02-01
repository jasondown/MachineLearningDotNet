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
    Chart.Line count |> Chart.WithStyling (Name = "Daily Count", Color = System.Drawing.Color.Green)
    Chart.Line (movingAvg 7 count) |> Chart.WithStyling (Name = "7 Day Moving Avg", Color = System.Drawing.Color.Blue)
    Chart.Line (movingAvg 30 count) |> Chart.WithStyling (Name = "30 Day Moving Avg", Color = System.Drawing.Color.Red) ]
    |> Chart.WithLegend (Title = "Legend")
    |> Chart.WithXAxis (Title = "Day Number")
    |> Chart.WithYAxis (Title = "Bikes Used")
    |> Chart.Show

let avg = data |> Seq.averageBy (fun x -> float x.Cnt)

let baseline = data |> Seq.averageBy (fun x -> abs (float x.Cnt - avg))

type Obs = Data.Row

let model (theta0, theta1) (obs : Obs) =
    theta0 + theta1 * (float obs.Instant)

//---------- Testing a linear combination
let model0 = model (avg, 0.)
let model1 = model (6000., -4.5)

Chart.Combine [
    Chart.Line count
    Chart.Line [ for obs in data -> model0 obs ] |> Chart.WithStyling (Name = "Model 0")
    Chart.Line [ for obs in data -> model1 obs ] |> Chart.WithStyling (Name = "Model 1") ]
    |> Chart.WithLegend (Title = "Legend")
    |> Chart.Show

type Model = Obs -> float

let cost (data : Obs seq) (m : Model) =
    data
    |> Seq.sumBy (fun x -> pown (float x.Cnt - m x) 2)
    |> sqrt

let overallCost = cost data
overallCost model0 |> printfn "Cost model0: %.0f"
overallCost model1 |> printfn "Cost model1: %.0f"

let update alpha (theta0, theta1) (obs : Obs) =
    let y = float obs.Cnt
    let x = float obs.Instant
    let theta0' = theta0 - 2. * alpha * 1. * (theta0 + theta1 * x - y)
    let theta1' = theta1 - 2. * alpha * x * (theta0 + theta1 * x - y)
    theta0', theta1'

let obs100 = data|> Seq.item 100
let testUpdate = update 0.00001 (0., 0.) obs100
cost [obs100] (model (0., 0.))
cost [obs100] (model testUpdate)

let stochastic rate (theta0, theta1) =
    data
    |>Seq.fold (fun (t0, t1) obs ->
        printfn "%.4f, %.4f" t0 theta1
        update rate (t0, t1) obs) (theta0, theta1)

let tuneRate =
    [ for r in 1 .. 20 ->
        (pown 0.1 r), stochastic (pown 0.1 r) (0., 0.) |> model |> overallCost ]

let rate = pown 0.1 8
let model2 = model (stochastic rate (0.0, 0.0))

Chart.Combine [
    Chart.Line count
    Chart.Line [ for obs in data -> model2 obs ] ] 
    |> Chart.Show

let hiRate = 10.0 * rate
let errorEval =
    data
    |> Seq.scan (fun (t0, t1) obs -> update hiRate (t0, t1) obs) (0.0, 0.0)
    |> Seq.map (model >> overallCost)
    |> Chart.Line
    |> Chart.Show

let batchUpdate rate (theta0, theta1) (data : Obs seq) =
    let updates =
        data
        |> Seq.map (update rate (theta0, theta1))
    let theta0' = updates |> Seq.averageBy fst
    let theta1' = updates |> Seq.averageBy snd
    theta0', theta1'

let batch rate iters =
    let rec search (t0, t1) i =
        if i = 0 
        then (t0, t1)
        else search (batchUpdate rate (t0, t1) data) (i-1)
    search (0.0, 0.0) iters

let batchedError rate =
    Seq.unfold (fun (t0, t1) ->
        let (t0', t1') = batchUpdate rate (t0, t1) data
        let err = model (t0, t1) |> overallCost
        Some(err, (t0', t1'))) (0.0, 0.0)
    |> Seq.take 100
    |> Seq.toList
    |> Chart.Line
    |> Chart.Show
