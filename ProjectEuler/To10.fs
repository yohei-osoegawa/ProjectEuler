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

let Problem8 =
    // より探索数が多ければ13桁の数の組み合わせと計算結果（22C13）をメモ化したほうがいいが、1000個程度の探索ならプルートフォースを行ったほうが良い
    let given ="7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
    given.ToCharArray() |> Array.windowed 13 |> Array.map (Array.map (fun x -> int64 x - 48L)) |> Array.map (Array.reduce ((*))) 
    |> Array.max

let Problem9 =
    // 複雑な2重ループを再帰で行うとややこしいので、素直にforを使う
    let mutable result = 0
    for a = 1 to 998 do
        for b = a + 1 to 1000 - 2 * a do
            let c = 1000 - a - b
            if a * a + b * b = c * c then result <- a * b * c
    result

open System.Linq
open System.Collections.Generic;
let Problem10 =
    // エラトステネスの篩を用いて素数表を作っていく。簡単のためふるい分けた値は０に変える
    let array = Enumerable.Range(2,1_999_999).ToArray()
    let max = 2_000_000 |> float |> sqrt |> int
    let rec yieldPfArray i =
        if i > max then ()
        elif array.[i-2] = 0 then yieldPfArray (i + 1)
        else
            let mutable j = 1
            while i * j <= 2_000_000 do
                array.[i*j-2] <- 0
            yieldPfArray (i + 1)
    yieldPfArray 2
    array.Select(int64).Sum()


