namespace Titanic

module Tree =
    
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

    //---------- Growing a tree from a sample and features

    let entropy label data = 
        let size = data |> Seq.length
        data 
        |> Seq.countBy label 
        |> Seq.map (fun (_, count) -> float count / float size)
        |> Seq.sumBy (fun f -> if f > 0. then - f * log f else 0.)

    let splitEntropy label feature data = 
        let knownData = 
            data 
            |> Seq.filter (fun obs -> feature obs |> Option.isSome)
        let size = knownData |> Seq.length
        knownData 
        |> Seq.groupBy feature
        |> Seq.sumBy (fun (_, group) ->
            let groupSize = group |> Seq.length
            let groupEntropy = group |> entropy label
            (float groupSize / float size) * groupEntropy) 

    let mostFrequentBy f sample =
        sample
        |> Seq.map f
        |> Seq.countBy id
        |> Seq.maxBy snd
        |> fst

    let rec growTree sample label features = 

        if (Map.isEmpty features)
        then sample |> mostFrequentBy label |> Answer
        else
            let (bestName,bestFeature) = 
                features
                |> Seq.minBy (fun kv -> 
                    splitEntropy label kv.Value sample)
                |> (fun kv -> kv.Key, kv.Value)
            let branches = 
                sample
                |> Seq.groupBy bestFeature
                |> Seq.filter (fun (value, group) -> value.IsSome)
                |> Seq.map (fun (value, group) -> value.Value,group)
            let defaultValue = 
                branches 
                |> Seq.maxBy (fun (value, group) -> 
                    group |> Seq.length)
                |> fst
            let remainingFeatures = features |> Map.remove bestName
            let nextLevel = 
                branches 
                |> Seq.map (fun (value, group) -> 
                    value, growTree group label remainingFeatures)
                |> Map.ofSeq

            Stump((bestName, bestFeature), defaultValue, nextLevel)