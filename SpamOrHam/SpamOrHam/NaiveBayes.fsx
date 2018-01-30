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

let validation, training = dataset.[..999], dataset.[1000..]

let tokenizeWords (text : string) =
    text.ToLowerInvariant()
    |> matchWords.Matches
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Value)
    |> Set.ofSeq

let casedTokenizer (text : string) =
    text
    |> matchWords.Matches
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Value)
    |> Set.ofSeq

let alltokens =
    training
    |> Seq.map snd
    |> vocabulary tokenizeWords

let casedTokens =
    training
    |> Seq.map snd
    |> vocabulary casedTokenizer

let top n (tokenizer : Tokenizer) (docs : string []) =
    let tokenized = docs |> Array.map tokenizer
    let tokens = tokenized |> Set.unionMany
    tokens
    |> Seq.sortBy (fun t -> - countIn tokenized t)
    |> Seq. take n
    |> Set.ofSeq

let ham, spam =
    let rawHam, rawSpam =
        training
        |> Array.partition (fun (lbl, _) -> lbl = Ham)
    rawHam |> Array.map snd,
    rawSpam |> Array.map snd

let hamCount = ham |> vocabulary casedTokenizer |> Set.count
let spamCount = spam |> vocabulary casedTokenizer |> Set.count

let topHam = ham |> top (hamCount / 10) casedTokenizer
let topSpam = spam |> top (spamCount / 10) casedTokenizer

let topTokens = Set.union topHam topSpam

let commonTokens = Set.intersect topHam topSpam
let specificTokens = Set.difference topTokens commonTokens

let rareTokens n (tokenizer : Tokenizer) (docs : string []) =
    let tokenized = docs |> Array.map tokenizer
    let tokens = tokenized |> Set.unionMany
    tokens
    |> Seq.sortBy (fun t -> countIn tokenized t)
    |> Seq.take n
    |> Set.ofSeq

let rareHam = ham |> rareTokens 50 casedTokenizer |> Seq.iter (printfn "%s")
let rareSpam = spam |> rareTokens 50 casedTokenizer |> Seq.iter (printfn "%s")

let phoneWords = Regex(@"0[7-9]\d{9}")
let phone (text : string) =
    match (phoneWords.IsMatch text) with
    | true -> "__PHONE__"
    | false -> text

let txtCode = Regex(@"\b\d{5}\b")
let txt (text : string) =
    match (txtCode.IsMatch text) with
    | true -> "__TXT__"
    | false -> text

let smartTokenizer = casedTokenizer >> Set.map phone >> Set.map txt

let smartTokens =
    specificTokens
    |> Set.add "__TXT__"
    |> Set.add "__PHONE__"

let createClassifier (tokenizer : Tokenizer) (tokens : Token Set) =
    train training tokenizer tokens

let alwaysHamClassifier (_ : string) = Ham
let txtClassifier = createClassifier tokenizeWords (["txt"] |> set)
let fullClassifier = createClassifier tokenizeWords alltokens
let casedClassifier = createClassifier casedTokenizer casedTokens
let topTokensCasedClassifier = createClassifier casedTokenizer topTokens
let topTokensCasedCommonRemovedClassifier = createClassifier casedTokenizer specificTokens
let smartTokensCasedClassifier = createClassifier smartTokenizer smartTokens

let validate (classifier : (string -> DocType)) (name : string) =
    validation
    |> Seq.averageBy (fun (doctType, sms) -> if doctType = classifier sms then 1.0 else 0.0)
    |> fun x -> x*100.
    |> printfn "Based on %s, correctly classified: %.2f%%" name

let betterValidation (classifier : (string -> DocType)) (name : string) =
    let hamAccuracy, falsePositive =
        validation
        |> Seq.filter (fun (docType, _) -> docType = Ham)
        |> Seq.averageBy (fun (docType, sms) ->
            if docType = classifier sms then 1.0 else 0.0)
        |> fun x -> x*100.
        |> fun x -> (x, 100. - x)
    printfn "Based on %s, correctly classified Ham: %.2f%% (fp: %.2f%%)" name hamAccuracy falsePositive
    
    let spamAccuracy, falseNegative =
        validation
        |> Seq.filter (fun (docType, _) -> docType = Spam)
        |> Seq.averageBy (fun (docType, sms) ->
            if docType = classifier sms then 1.0 else 0.0)
        |> fun x -> x*100.
        |> fun x -> (x, 100. - x)
    printfn "Based on %s, correctly classified Spam: %.2f%% (fn: %.2f%%)" name spamAccuracy falseNegative
    printfn "-----" |> ignore

// Compare classifiers ... Gotta be a better way to get the name
betterValidation alwaysHamClassifier "alwaysHamClassifier"
betterValidation txtClassifier "txtClassifier"
betterValidation fullClassifier "fullClassifier"
betterValidation casedClassifier "casedClassifier"
betterValidation topTokensCasedClassifier "topTokensCasedClassifier"
betterValidation topTokensCasedCommonRemovedClassifier "topTokensCasedCommonRemovedClassifier"
betterValidation smartTokensCasedClassifier "smartTokensCasedClassifier"
