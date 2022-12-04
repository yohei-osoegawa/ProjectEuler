module No2

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