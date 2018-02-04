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

    let initialize observations k = 
        let size = Array.last observations

        let centroids = 
            pickFrom size k
            |> Array.mapi (fun i index ->
                i+1, observations.[index])

        let assignments =
            observations
            |> Array.map (fun x -> 0, x)

        assignments, centroids