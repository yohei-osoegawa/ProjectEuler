module To10

open Microsoft.FSharp.Core.Operators.Checked

let Problem1 :int =
    [1..999] |> List.filter(fun x -> x % 3 = 0 || x % 5 = 0 ) |> List.sum

let Problem2 :int = 
    let rec FibList (acc1, acc2) = [
        let v = acc1 + acc2
        if 4_000_000 < v then ()
        else
            yield v
            yield! FibList(acc2, v)
    ]

    FibList (0, 1)
    |> List.filter (fun x -> x % 2 = 0)
    |> List.sum