#load "NaiveBayes.fs"
open NaiveBayes.Classifier

open System.IO
open System.Text.RegularExpressions
open System.Web.UI.WebControls

type DocType =
    | Ham
    | Spam

let parseDocType (label : string) =
    match label with
    | "ham"     -> Ham
    | "spam"    -> Spam
    | _         -> failwith "Unknown label"

let parseLine (line : string) =
    let split = line.Split '\t'
    let label = split.[0] |> parseDocType
    let message = split.[1]
    (label, message)

let filename = "SMSSpamCollection"
let path = Path.Combine(__SOURCE_DIRECTORY__, @"..\Data\" + filename)

let dataset =
    File.ReadAllLines path
    |> Array.map parseLine

let matchWords = Regex(@"\w+")

let tokenizeWords (text : string) =
    text.ToLowerInvariant()
    |> matchWords.Matches
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Value)
    |> Set.ofSeq

let validation, training = dataset.[..999], dataset.[1000..]

let txtClassifier = train training tokenizeWords (["txt"] |> set)

validation
|> Seq.averageBy (fun (doctType, sms) -> if doctType = txtClassifier sms then 1.0 else 0.0)
|> fun x -> x*100.
|> printfn "Based on 'txt', correctly classified: %.3f%%"