#I @"..\packages\"
#r @"FSharp.Data.2.4.4\lib\net45\FSharp.Data.dll"

open FSharp.Data

type Data = CsvProvider<"day.csv">
let dataset = Data.GetSample()
let data = dataset.Rows
