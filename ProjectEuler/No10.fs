module No10
open System.Linq
open System.Collections.Generic

/// 200万以下の全ての素数の和 
let Problem10 =
    // エラトステネスの篩を用いて素数表を作っていく。
    // 最初はarrayを使っていたが、合致する数字を削除するといった操作はsetのほうが適切なのでsetを使う
    let set = new HashSet<int>(Enumerable.Range(2,1_999_999))
    let max = 2_000_000 |> float |> sqrt |> int
    let rec yieldPfSet i =
        if i > max then ()
        else
            // この箇所で、jを１ずつ増えるのではなく作られ途中の素数表の中にある数は除外することで高速化できると思う
            for j = 2 to 2_000_000 / i do // 素数の倍数は除くが自身は除いてはならない
                set.Remove(i*j) |> ignore
            yieldPfSet (i + 1)
    yieldPfSet 2
    set.Select(int64).Sum()