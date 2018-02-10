namespace Titanic

module Tree =
    open FSharp.Data.HtmlAttribute
    
    type Feature<'a> = 'a -> string Option
    type NamedFeature<'a> = string * Feature<'a>
    type Label<'a, 'b when 'b:equality> = 'a -> 'b

    type Tree<'a, 'b when 'b:equality> =
        | Answer of 'b
        | Stump of NamedFeature<'a> * string * Map<string, Tree<'a, 'b>>

    let rec decide tree observation =
        match tree with
        | Answer labelValue -> labelValue
        | Stump ((featureName, feature), valueWhenMissing, branches) ->
            let featureValue = feature observation
            let usedValue =
                match featureValue with
                | None -> valueWhenMissing
                | Some value -> 
                    match (branches.TryFind value) with
                    | None -> valueWhenMissing
                    | Some _ -> value
            let nextLevelTree = branches.[usedValue]
            decide nextLevelTree observation
