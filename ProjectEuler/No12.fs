module No12

/// 約数の個数が５００を超える最初の三角数
let Problem12 =
    // 方針：約数の個数を求めてそれが500より大きいか判断する関数を別途定義し、find関数使って、trueの時の最初の値を取得する

    // Note:三角数は(n+1)C2で表される。また約数の個数は素因数分解から求められる
    // ここはメモ化で高速化の余地がある
    let rec pfd n i = 
        let max = n |> float |> sqrt |> int
        if i > max then [n] 
        elif n % i = 0 then i :: pfd (n/i) i
        else pfd (n) (i+1)

    let doesAnsPair (n, m) =
        [n; m]
        |> List.map (fun i -> pfd i (2))
        |> List. collect id
        |> List.countBy id
        |> List.map (fun (x, count) -> if x = 2 then count - 1 else count)
        |> List.fold (fun acc next -> acc * (next + 1)) (1)
        |> (<) 500 

    // ここは表記方法に改善の余地あるかも
    seq{
        let mutable i = 2
        while true do
            yield i
            i<-i+1
    } 
    |> Seq.pairwise
    |> Seq.find doesAnsPair
    |> fun (n1, n2) -> n1 * n2 / 2 