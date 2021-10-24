(*
// Zadanie 1
// z³o¿onoœæ czasowa liniowa O(n), gdzie n jest d³ugoœci¹ listy wejœciowej
// z³o¿onoœæ pamieciowa liniowa O(n), gdzie n jest d³ugoœci¹ listy wejœciowej
*)
let rec splitBySign list =
    if list = [] then
        ([], [])
    else
        let (lower, higher) = splitBySign (List.tl list) in
        if List.hd list < 0 then
            ((List.hd list) :: lower, higher)
        else if List.hd list mod 2 = 1 then
            (lower, (List.hd list) :: higher)
        else
            (lower, higher);;

splitBySign [-3; -6; 7; -9; 13];;
splitBySign [];;
splitBySign [1; 2; -1; -2; -3; 3; 4; -4; -5; 5];;

(*
// Zadanie 2
// z³o¿onoœæ czasowa liniowa O(n), gdzie n jest d³ugoœci¹ listy wejœciowej
// z³o¿onoœæ pamiêciowa sta³a O(1)
*)
let listLength list =
    let rec calculateLength list i =
        if list = [] then i
        else calculateLength (List.tl list) (i + 1)

    in calculateLength list 0;;

listLength [5; 4; 3; 2];;
listLength ["A"; "B"; "C"];;
listLength [];;


(*
// Zadanie 3
// z³o¿onoœæ czasowa liniowa O(n), gdzie n jest d³ugoœci¹ listy wejœciowej
// z³o¿onoœæ pamiêciowa liniowa O(n), gdzie n jest d³ugoœci¹ krótszej listy wejœciowej
*)

let rec joinLists listA listB =
    match(listA, listB) with
        ([], _) -> listB
        | (_, _) ->
            List.hd listA :: joinLists listB (List.tl listA);;

joinLists [5; 4; 3; 2] [1; 2; 3; 4; 5; 6];;
joinLists [] [];;
joinLists [] [5; 4; 3; 2; 1];;
joinLists [5; 4; 3; 2; 1] [];;
