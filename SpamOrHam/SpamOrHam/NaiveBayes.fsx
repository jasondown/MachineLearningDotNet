#load "NaiveBayes.fs"
open NaiveBayes.Classifier

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

let alltokens =
    training
    |> Seq.map snd
    |> vocabulary tokenizeWords

let createClassifier (tokenizer : Tokenizer) (tokens : Token Set) =
    train training tokenizer tokens

let alwaysHamClassifier (_ : string) = Ham
let txtClassifier = createClassifier tokenizeWords (["txt"] |> set)
let fullClassifier = createClassifier tokenizeWords alltokens

let validate (classifier : (string -> DocType)) (name : string) =
    validation
    |> Seq.averageBy (fun (doctType, sms) -> if doctType = classifier sms then 1.0 else 0.0)
    |> fun x -> x*100.
    |> printfn "Based on %s, correctly classified: %.3f%%" name

// Compare classifiers
validate alwaysHamClassifier "alwaysHamClassifier"
validate txtClassifier "txtClassifier"
validate fullClassifier "fullClassifier"