module No5

open System.Linq

let Problem5 =
    //2から20まで素因数分解を行って、それらを組み合わせる
    let rec pf num div =
        if ((float num) |> sqrt  |> int) < div then [num]
        elif num % div = 0 then div :: pf (num / div) div
        else pf num (div + 1)

    let allPfList = 
        [2..20] |> List.map (fun x -> pf x 2)
        |> List.map (fun x -> x|> List.countBy id)
        |> List.toArray
        |> Array.reduce (fun acc x -> acc @ x)

    let toDict =
        let mutable map = new System.Collections.Generic.Dictionary<_,_>()
        for (num, count) in allPfList do
            match map.TryGetValue(num) with
            | (true, count2) -> map.[num] <- max count count2
            | (false, _) -> map.Add(num,count)
        map

    // Hack:今見たら、reduce使ったほうが良い気がする
    toDict.Aggregate( 1,fun  acc pair -> acc * (pown pair.Key pair.Value))