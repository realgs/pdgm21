let splitBySign xs =
    let rec splitBySignIter(list, negative, oddPostive) =
        match list with
            | [] -> (negative, oddPostive)
            | h :: t when h < 0 -> splitBySignIter(t, negative @ [h], oddPostive)
            | h :: t when h mod 2 <> 0 -> splitBySignIter(t, negative, oddPostive @ [h])
            | _ -> splitBySignIter(List.tl list, negative, oddPostive)
    in splitBySignIter(xs, [], []);;

splitBySign [-3;-6;7;-9;13];;
splitBySign [-3;-6;7;-9;12];;
splitBySign [-3;-6;7;-9;0;0;0];;
splitBySign [];;


let rec lengthOfList list =
    match list with
        | [] -> 0
        | h :: t -> 1 + lengthOfList t;;

lengthOfList [5; 4; 3; 2];;
lengthOfList [-3;-6;7;-9;12];;
lengthOfList [-3;-6;7;-9;0;0;0];;
lengthOfList [];;
lengthOfList [0];;


let rec joinLists list1 list2 =
    match list1, list2 with
        | ([], []) -> []
        | (_, []) -> list1
        | ([], _) -> list2
        | (h1 :: t1, h2 :: t2) -> h1 :: h2 :: joinLists t1 t2;;

joinLists [5;4;3;2] [1;2;3;4;5;6];;
joinLists [-3;-6;7;-9;0;0;0] [-3;-6;7;-9;12];;
joinLists [] [0;2;5;7;2];;
joinLists [0;876;2;5;7;2] [];;
joinLists [0;876;2;5;7;2] [1;2];;
joinLists [0;876;2;5;7;2] [-1];;
