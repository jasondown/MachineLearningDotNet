namespace Unsupervised

module KMeans =
    
    let pickFrom size k =
        let rng = System.Random()
        let rec pick (set : int Set) =
            let candidate = rng.Next(size)
            let set = set.Add candidate
            if set.Count = k then set
            else pick set
        pick Set.empty |> Set.toArray