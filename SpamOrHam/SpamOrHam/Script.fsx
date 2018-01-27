open System.IO

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