module No1

let Problem1 :int =
    [1..999] |> List.filter(fun x -> x % 3 = 0 || x % 5 = 0 ) |> List.sum
