module No9

/// (a^2 + b^2 = c^2) & (a + b + c = 1000) となるようなabcの積
let Problem9 =
    // 複雑な2重ループを再帰で行うとややこしいので、素直にforを使う
    let mutable result = 0
    for a = 1 to 998 do
        for b = a + 1 to 1000 - 2 * a do
            let c = 1000 - a - b
            if a * a + b * b = c * c then result <- a * b * c
    result
