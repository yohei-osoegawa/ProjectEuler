module To10

open Microsoft.FSharp.Core.Operators.Checked

let Problem1 :int =
    [1..999] |> List.filter(fun x -> x % 3 = 0 || x % 5 = 0 ) |> List.sum

let Problem2 :int = 
    // たぶん、3n-1項のみを出力して計算したほうがクレバー
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

let Problem2_1 :int = 
    // 末尾再帰なしのfib数列
    let rec FibList (acc1, acc2) = 
        if 4_000_000 < acc2 then []
        else acc2 :: FibList (acc2, acc1 + acc2) 
    
    FibList (0, 1)
    |> List.filter (fun x -> x % 2 = 0)
    |> List.sum

let Problem3 =
    let given = 600851475143L
    let rec pf num div =
        if ((float num) |> sqrt  |> int64) < div then [num]
        elif num % div = 0L then div :: pf (num / div) div
        else pf num (div + 1L)
    pf given 2L |> List.last

let Problem4 =
    let cand =
        (seq{999..-1..0}, seq{999..-1..0}) ||> Seq.allPairs
        |> Seq.sortByDescending (fun (x,y) -> x * y)
        |> Seq.find ( fun (x, y) -> 
            let str = string (x * y)
            let revStr = str.ToCharArray() |> Array.rev |>fun x -> new System.String(x)
            str = revStr)
        |> fun (x,y) -> x*y
    
    0

    
        