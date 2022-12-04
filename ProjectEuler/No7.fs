module No7

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
