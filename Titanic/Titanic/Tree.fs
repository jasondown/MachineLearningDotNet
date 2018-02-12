namespace Titanic

module Tree =
    open System
    
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

    let rec display depth tree =
        let padding = String.replicate (2 * depth) " "
        match tree with
        | Answer label -> printfn " -> %A" label
        | Stump ((name, _), _, branches) ->
            printfn ""
            branches
            |> Seq.iter (fun kv ->
                printf "%s ? %s : %s" padding name kv.Key
                display (depth + 1) kv.Value)

//----------Avoid over-fitting. Inject feature filters.
    let entropyGainFilter sample label feature =
        splitEntropy label feature sample - entropy label sample < 0.

    let leafSizeFilter minSize sample label feature =
        sample
        |> Seq.map feature
        |> Seq.choose id
        |> Seq.countBy id
        |> Seq.forall (fun (_, groupSize) -> groupSize > minSize)

    let rec growTree2 filters sample label features = 

        let features =
            features
            |> Map.filter (fun name feature -> 
                filters |> Seq.forall (fun filter -> filter sample label feature)) 

        if (Map.isEmpty features)
        then sample |> mostFrequentBy label |> Answer
        else
            let (bestName, bestFeature) = 
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
                    value, growTree2 filters group label remainingFeatures)
                |> Map.ofSeq

            Stump((bestName, bestFeature), defaultValue, nextLevel)

    //----------Forest:
    //               Random feature (without repitition)
    //               Random examples from sample (with repitition)
    //               Combine trees into majority vote
    let pickRepeat (rng : Random) proportion original =
        let size = original |> Array.length
        let sampleSize = proportion * float size |> int
        Array.init sampleSize (fun _ -> original.[rng.Next(size)])

    let pickNoRepeat (rng : Random) proportion original =
        let size = original |> Seq.length
        let sampleSize = proportion * float size |> int

        let init = ([], size)
        original
        |> Seq.fold (fun (sampled, remaining) item ->
            let picked = List.length sampled
            let p = float (sampleSize - picked) / float remaining
            if (rng.NextDouble() <= p)
            then (item::sampled, remaining - 1)
            else (sampled, remaining - 1)) init
        |> fst

    let predict forest observation =
        forest
        |> Seq.map (fun tree -> decide tree observation)
        |> Seq.countBy id
        |> Seq.maxBy snd
        |> fst

    let growForest size sample label features =
        let rng = Random ()

        let propFeatures =
            let total = features |> Seq.length |> float
            sqrt total / total

        let featSample () = pickNoRepeat rng propFeatures features
        let popSample () = pickRepeat rng 1.0 sample
        let filters = [ leafSizeFilter 10; entropyGainFilter ]

        let forest = [
            for _ in 1 .. size ->
                let sample = popSample ()
                let features = featSample () |> Map.ofList
                growTree2 filters sample label features ]

        predict forest