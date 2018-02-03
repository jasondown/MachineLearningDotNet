#I @"..\packages\"
#r @"FSharp.Charting.0.91.1\lib\net45\FSharp.Charting.dll"
#r @"FSharp.Data.2.4.4\lib\net45\FSharp.Data.dll"
#r @"MathNet.Numerics.3.20.2\lib\net40\MathNet.Numerics.dll"
#r @"MathNet.Numerics.FSharp.Signed.3.20.2\lib\net40\MathNet.Numerics.FSharp.dll"

open FSharp.Charting
open FSharp.Data
open MathNet
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double

//---------- Playing with vectors and matrices
let A = vector [ 1.; 2.; 3. ]
let B = matrix [ [ 1.; 2. ]
                 [ 3.; 4. ]
                 [ 5.; 6. ] ]
let C = A * A
let D = A * B
let E = A * B.Column(1)


//---------- Doing same calculations as in script.fsx, but with nicer linear algebra calculations.
type Data = CsvProvider<"day.csv">
let dataset = Data.Load("day.csv")
let data = dataset.Rows

type Vec = Vector<float>
type Mat = Matrix<float>

let cost (theta : Vec) (Y : Vec) (X : Mat) =
    let ps = Y - (theta * X.Transpose())
    ps * ps |> sqrt

let predict (theta : Vec) (v: Vec) = theta * v

let X = matrix [ for obs in data -> [ 1.; float obs.Instant ]]
let Y = vector [ for obs in data -> float obs.Cnt ]

let theta = vector [6000.; -4.5]

predict theta (X.Row(0))
cost theta Y X

let estimate (Y : Vec) (X : Mat) =
    (X.Transpose() * X).Inverse() * X.Transpose() * Y

#time
let theta_normal = (X.Transpose() * X).Inverse() * X.Transpose() * Y
#time

//----------Testing MKL
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

open MathNet.Numerics
open MathNet.Numerics.Providers.LinearAlgebra.Mkl

// Shuffle to prevent seasonal data bias
let seed = 314159
let rng = System.Random(seed)

// Fisher-Yates shuffle
let shuffle (arr : 'a []) =
    let arr = Array.copy arr
    let l = arr.Length 
    for i in (l-1) .. -1 .. 1 do
        let temp = arr.[i]
        let j = rng.Next(0, i+1)
        arr.[i] <- arr.[j]
        arr.[j] <- temp
    arr

let training, validation =
    let shuffled =
        data
        |> Seq.toArray
        |> shuffle
    let size = 
        0.7 * float (Array.length shuffled) |> int
    shuffled.[..size],
    shuffled.[size+1..]

type Obs = Data.Row
type Model = Obs -> float
type Featurizer = Obs -> float list

let predictor (f : Featurizer) (theta : Vec) =
    f >> vector >> (*) theta

let evaluate (model : Model) (data : Obs seq) =
    data
    |> Seq.averageBy (fun obs -> abs (model obs - float obs.Cnt))

let model (f : Featurizer) (data : Obs seq) =
    let Yt, Xt =
        data
        |> Seq.toList
        |> List.map (fun obs -> float obs.Cnt, f obs)
        |> List.unzip
    let theta = estimate (vector Yt) (matrix Xt)
    let predict = predictor f theta 
    theta, predict

let featurizer0 (obs : Obs) =
    [ 1.; float obs.Instant; ]

let (theta0, model0) = model featurizer0 training

// Evaluate the quality of our model0 on the training and validation sets
evaluate model0 training |> printfn "Training: %.0f"
evaluate model0 validation |> printfn "Validation: %.0f"
Chart.Combine [
    Chart.Line [ for obs in data -> float obs.Cnt ]
    Chart.Line [ for obs in data -> model0 obs ] ]
    |> Chart.Show

//----------Add some features to our model
let featurizer1 (obs : Obs) =
    [ 1.
      obs.Instant   |> float
      obs.Atemp     |> float
      obs.Hum       |> float
      obs.Temp      |> float
      obs.Windspeed |> float
    ]

let (theta1, model1) = model featurizer1 training

evaluate model1 training |> printfn "Training: %.0f"
evaluate model1 validation |> printfn "Validation: %.0f"

Chart.Combine [
    Chart.Line [ for obs in data -> float obs.Cnt ] |> Chart.WithStyling (Name = "Daily Count")
    Chart.Line [ for obs in data -> model0 obs ]    |> Chart.WithStyling (Name = "Model0 - 1 Feature")
    Chart.Line [ for obs in data -> model1 obs ]    |> Chart.WithStyling (Name = "Model1 - 5 Features")
]   |> Chart.WithLegend (Title = "Legend")
    |> Chart.WithTitle (Text = "Using Linear Algebra For Prediction", 
                        Color = System.Drawing.Color.Red,
                        FontStyle = System.Drawing.FontStyle.Bold,
                        FontSize = 20.)
    |> Chart.Show

Chart.Combine [
    Chart.Point [ for obs in data -> float obs.Cnt, model0 obs ] |> Chart.WithStyling (Name = "Model0 - 1 Feature")
    Chart.Point [ for obs in data -> float obs.Cnt, model1 obs ] |> Chart.WithStyling (Name = "Model1 - 5 Features", Color = System.Drawing.Color.Red)
]   |> Chart.WithLegend (Title = "Legend")
    |> Chart.WithTitle (Text = "Prediction VS Actual Scatter Plot", 
                        Color = System.Drawing.Color.Red,
                        FontStyle = System.Drawing.FontStyle.Bold,
                        FontSize = 16.)
    |> Chart.Show

//---------- Handling Categorical Features
let featurizer2 (obs : Obs) =
    [ 1.
      obs.Instant   |> float
      obs.Hum       |> float
      obs.Temp      |> float
      obs.Windspeed |> float
      // Using Sunday as a reference and avoid collinearity problem (if obs.Weekday = 0 then 1.0 else 0.0)
      (if obs.Weekday = 1 then 1.0 else 0.0)
      (if obs.Weekday = 2 then 1.0 else 0.0)
      (if obs.Weekday = 3 then 1.0 else 0.0)
      (if obs.Weekday = 4 then 1.0 else 0.0)
      (if obs.Weekday = 5 then 1.0 else 0.0)
      (if obs.Weekday = 6 then 1.0 else 0.0)
    ]

let (theta2, model2) = model featurizer2 training

evaluate model2 training |> printfn "Training: %.0f"
evaluate model2 validation |> printfn "Validation: %.0f"

Chart.Combine [
    Chart.Line [ for obs in data -> float obs.Cnt ] |> Chart.WithStyling (Name = "Daily Count")
    Chart.Line [ for obs in data -> model0 obs ]    |> Chart.WithStyling (Name = "Model0 - 1 Feature")
    Chart.Line [ for obs in data -> model1 obs ]    |> Chart.WithStyling (Name = "Model1 - 5 Features")
    Chart.Line [ for obs in data -> model2 obs ]    |> Chart.WithStyling (Name = "Model1 - 5 Features + Days")
]   |> Chart.WithLegend (Title = "Legend")
    |> Chart.WithTitle (Text = "Using Linear Algebra For Prediction", 
                        Color = System.Drawing.Color.Red,
                        FontStyle = System.Drawing.FontStyle.Bold,
                        FontSize = 20.)
    |> Chart.Show

Chart.Combine [
    //Chart.Point [ for obs in data -> float obs.Cnt, model0 obs ] |> Chart.WithStyling (Name = "Model0 - 1 Feature")
    Chart.Point [ for obs in data -> float obs.Cnt, model1 obs ] |> Chart.WithStyling (Name = "Model1 - 5 Features", Color = System.Drawing.Color.Red)
    Chart.Point [ for obs in data -> float obs.Cnt, model2 obs ] |> Chart.WithStyling (Name = "Model2 - 5 Features + Days", Color = System.Drawing.Color.Blue)
]   |> Chart.WithLegend (Title = "Legend")
    |> Chart.WithTitle (Text = "Prediction VS Actual Scatter Plot", 
                        Color = System.Drawing.Color.Red,
                        FontStyle = System.Drawing.FontStyle.Bold,
                        FontSize = 16.)
    |> Chart.Show

//---------- Bicycle usage against temperature usage scatterplot
Chart.Point [ for obs in data -> obs.Temp, obs.Cnt ] |> Chart.Show