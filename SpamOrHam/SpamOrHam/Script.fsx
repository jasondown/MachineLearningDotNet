open System.IO
open System.Text.RegularExpressions

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
let path = __SOURCE_DIRECTORY__ + @"..\..\Data\" + filename

let dataset =
    File.ReadAllLines path
    |> Array.map parseLine

let matchWords = Regex(@"\w+")

let tokens (text : string) =
    text.ToLowerInvariant()
    |> matchWords.Matches
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Value)
    |> Set.ofSeq

//----------------------------
let spamWithFREE =
    dataset
    |> Array.filter (fun (docType, _) -> docType = Spam)
    |> Array.filter (fun (_, sms) -> sms.Contains("FREE"))
    |> Array.length

let hamWithFREE =
    dataset
    |> Array.filter (fun (docType, _) -> docType = Ham)
    |> Array.filter (fun (_, sms) -> sms.Contains("FREE"))
    |> Array.length

let primitiveClassifier (sms : string) =
    if (sms.Contains "FREE")
    then Spam
    else Ham