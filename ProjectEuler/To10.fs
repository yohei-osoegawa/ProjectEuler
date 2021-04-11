module To10

open Microsoft.FSharp.Core.Operators.Checked
open System.Linq

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
    (seq{999..-1..0}, seq{999..-1..0}) ||> Seq.allPairs
    |> Seq.sortByDescending (fun (x,y) -> x * y)
    |> Seq.find ( fun (x, y) -> 
        let str = string (x * y)
        let revStr = str.ToCharArray() |> Array.rev |>fun x -> new System.String(x)
        str = revStr)
    |> fun (x,y) -> x*y

    
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

    toDict.Aggregate( 1,fun  acc pair -> acc * (pown pair.Key pair.Value))

let Problem6 =
    pown (100*101/2) 2 - 100*101*201/6

let Problem7 = 
    // 素朴なやり方だが、末尾再帰になってるはず。
    let rec calc list next i =
        if i >= 10001 then list |> List.head
        else 
            let rec find num =
                if (list |> List.exists (fun x -> num % x = 0)) then find (num+1)
                else num
            let added = find next  
            calc ((added) :: list) (added+1) (i+1)
    calc [] 2 0

/// given内の隣接する13の数の総乗の最大値
let Problem8 =
    // より探索数が多ければ13桁の数の組み合わせと計算結果（22C13）をメモ化したほうがいいが、1000個程度の探索ならプルートフォースを行ったほうが良い
    let given ="7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
    given.ToCharArray() |> Array.windowed 13 |> Array.map (Array.map (fun x -> int64 x - 48L)) |> Array.map (Array.reduce ((*))) 
    |> Array.max

/// (a^2 + b^2 = c^2) & (a + b + c = 1000) となるようなabcの積
let Problem9 =
    // 複雑な2重ループを再帰で行うとややこしいので、素直にforを使う
    let mutable result = 0
    for a = 1 to 998 do
        for b = a + 1 to 1000 - 2 * a do
            let c = 1000 - a - b
            if a * a + b * b = c * c then result <- a * b * c
    result

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

let Problem11 =
    let given =
        """08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48"""
    // ジャグ配列へ
    let givenArray = given.Split() |> Array.chunkBySize(20) |> Array.map (Array.map int)
    // 縦4つ取得
    let collumnChunk = [
        for i in 0..19 do for j in 0..16 -> givenArray.[j..(j+3)] |> Array.map(fun x -> x.[i])|> Array.toList
    ]
    // 横4つ取得
    let rowChunk =[
        for j in 0..19 do for i in 0..16 -> givenArray.[j].[i..(i+3)] |> Array.toList
    ]
    // 斜め4つ取得
    let crossChunk1 =[
        for i in 0..16 do for j in 0..16 -> [0..3] |> List.map (fun x ->givenArray.[j+x].[i+x])
    ]
    let crossChunk2 =[
        for i in 0..16 do for j in 0..16 -> [0..3] |> List.map (fun x ->givenArray.[j+x].[i+(3-x)])
    ]

    // 上記の積のうち最大値を求める
    [
        yield! collumnChunk
        yield! rowChunk
        yield! crossChunk1
        yield! crossChunk2
    ] |> List.map(fun [x1;x2;x3;x4] -> x1*x2*x3*x4)
    |> List.max



