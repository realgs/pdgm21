let reverse xs =
    let rec reverseIter xs result =
        if xs = [] then result
        else reverseIter (List.tl xs) (List.hd xs :: result)
        in
    reverseIter xs [];;

let splitBySign xs =
    let rec splitBySignIter xs pos neg =
        match xs with
        | [] -> (reverse neg, reverse pos)
        | h::t ->
            if h < 0 then splitBySignIter t pos (h :: neg)
            else if h mod 2 <> 0 then splitBySignIter t (h :: pos) neg
            else splitBySignIter t pos neg
        in
    splitBySignIter xs [] [];;

splitBySign [-3;-6;7;-9;13] = ([-3;-6;-9], [7;13]);;
splitBySign [-5;-4;-3;-2;-1;0;1;2;3;4;5] = ([-5;-4;-3;-2;-1], [1;3;5]);;
splitBySign [] = ([], []);;

let lengthOfList xs =
    let rec lengthOfListIter xs acc =
        if xs = [] then acc
        else lengthOfListIter (List.tl xs) (acc + 1)
        in
    lengthOfListIter xs 0;;

lengthOfList [5;4;3;2] = 4;;
lengthOfList [1;2;3;4;5;6;7;8;9] = 9;;
lengthOfList [] = 0;;

let joinLists xs ys =
    let rec joinListsIter xs ys result =
        match (xs, ys) with
        | ([], []) -> reverse result
        | ([], ys) -> (reverse result) @ ys
        | (xs, []) -> (reverse result) @ xs
        | (h1::t1, h2::t2) -> joinListsIter t1 t2 (h2::h1::result)
        in
    joinListsIter xs ys [];;

joinLists [5;4;3;2] [1;2;3;4;5;6] = [5;1;4;2;3;3;2;4;5;6];;
joinLists [] [1;2;3] = [1;2;3];;
joinLists [1;2;3] [] = [1;2;3];;
joinLists [] [] = [];;
joinLists [1] [1] = [1;1];;
