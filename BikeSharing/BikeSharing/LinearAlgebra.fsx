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