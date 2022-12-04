module No3

let Problem3 =
    let given = 600851475143L
    let rec pf num div =
        if ((float num) |> sqrt  |> int64) < div then [num]
        elif num % div = 0L then div :: pf (num / div) div
        else pf num (div + 1L)
    pf given 2L |> List.last