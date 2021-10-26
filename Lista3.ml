(* Szymon Sawczuk *)

(* Helping functions *)
let reverseList list = 
  let rec reverseListIter(list, reversed) =
    if list = [] then reversed
    else reverseListIter(List.tl list, List.hd list::reversed)
  in reverseListIter(list, []);;

(* Zadanie 1 *)
let splitBySign list = 
  let rec splitBySignIter(list, list1, list2) =
    if list = [] then (reverseList list1, reverseList list2)
    else splitBySignIter(List.tl list, 
    (if List.hd list < 0 then List.hd list::list1 else list1), 
    (if List.hd list > 0 && List.hd list mod 2 <> 0 then List.hd list::list2 else list2))
  in splitBySignIter(list, [], []);;

splitBySign [-3; -6; 7; -9; 13];;
splitBySign [];;
splitBySign [2; 0; -3; 3; 5; -2];; 

(* Zadanie 2 *)
let lengthOfList list = 
  let rec lengthOfListIter(list, result) = 
    if list = [] then result
    else lengthOfListIter(List.tl list, result + 1)
  in lengthOfListIter(list, 0);;

lengthOfList [5; 4; 3; 2];;
lengthOfList ["Ala"; "ma"; "kota"];;
lengthOfList [];;

(* Zadanie 3 *)
let joinLists(list1, list2) = 
  let rec joinListsIter(list1, list2, resultList) = 
    match (list1, list2) with
      (head1::tail1, head2::tail2) -> joinListsIter(tail1, tail2, head2::head1::resultList)
    |  _ -> reverseList(resultList) @ (if list1 != [] then list1 else list2)
  in joinListsIter(list1, list2, []);;

joinLists ([5; 4; 3; 2], [1; 2; 3; 4; 5; 6]);;
joinLists([], []);;
joinLists([1; 2; 3], []);;
joinLists(["Ala"; "kota"], ["ma"; "!"]);;