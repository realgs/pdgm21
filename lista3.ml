(* Szymon Bak *)

(* zadanie 1 *)
let splitBySign list =
    let rec splitBySignHelper (list, listNegative, listPositive) = 
        if list = [] then (listNegative, listPositive)
        else if List.hd list < 0 then splitBySignHelper(List.tl list, listNegative @ [List.hd list], listPositive)
        else if List.hd list mod 2 <> 0 then splitBySignHelper(List.tl list, listNegative, listPositive @ [List.hd list])
        else splitBySignHelper(List.tl list, listNegative, listPositive)
    in splitBySignHelper(list, [], []);;

splitBySign([-3; -6; 7; -9; 13]) = ([-3; -6; -9], [7; 13]);;
splitBySign([1; 1; 1; 3; 5;]) = ([], [1; 1; 1; 3; 5]);;
splitBySign([]) = ([], []);;

(* zadanie 2 *)
let rec lengthOfList list = 
    if list = [] then 0
    else 1 + lengthOfList(List.tl list);;

lengthOfList([5;4;3;2]) = 4;;
lengthOfList([]) = 0;;
lengthOfList([1; 2; 3; 4; 5; 6; 7; 8; 9; 10]) = 10;;

(* zadanie 3 *)
let rec joinLists(list1, list2) =
    match (list1, list2) with
            ([], []) -> []
        |   (_ , []) -> list1
        |   ([], _)  -> list2
        |   (h1::t1, h2::t2) -> h1 :: h2 :: joinLists(t1, t2);;

joinLists([5;4;3;2], [1;2;3;4;5;6]) = [5; 1; 4; 2; 3; 3; 2; 4; 5; 6];;
joinLists([1; 2; 3; 4], [-1; -2; -3; -4]) = [1; -1; 2; -2; 3; -3; 4; -4];;
joinLists(['a'; 'b'; 'c'], []) = ['a'; 'b'; 'c'];;
joinLists([], ['a'; 'b'; 'c']) = ['a'; 'b'; 'c'];;
joinLists([], []) = [];;