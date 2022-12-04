module No4

let Problem4 =
    (seq{999..-1..0}, seq{999..-1..0}) ||> Seq.allPairs
    |> Seq.sortByDescending (fun (x,y) -> x * y)
    |> Seq.find ( fun (x, y) -> 
        let str = string (x * y)
        let revStr = str.ToCharArray() |> Array.rev |>fun x -> new System.String(x)
        str = revStr)
    |> fun (x,y) -> x*y
