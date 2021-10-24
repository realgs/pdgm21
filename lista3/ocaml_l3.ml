(* 1 *)
let rec splitBySign list = (
  let rec negativeList list = (
    if list = [] then []
    else if List.hd list < 0 then (List.hd list)::negativeList(List.tl list)
    else negativeList(List.tl list)
  ) in
  let rec positiveOddList list = (
    if list = [] then []
    else if List.hd list > 0 && List.hd list mod 2 == 1 then (List.hd list)::positiveOddList(List.tl list)
    else positiveOddList(List.tl list)
  ) in
  (negativeList(list), positiveOddList(list))
);;

splitBySign [-3; -6; 7; -9; 13];;
splitBySign [1; 2; -3; 0; 5];;
splitBySign [];;


(* 2 *)
let rec lengthOfList list =
  if list = [] then 0
  else lengthOfList(List.tl list) + 1
;;

lengthOfList [5; 4; 3; 2];;
lengthOfList ["Ala"; "ma"; "kota"];;
lengthOfList [];;


(* 3 *)
let rec joinLists (list1, list2) =
  match (list1, list2) with
    ([], []) -> []
  | ([], _) -> list2
  | (_, []) -> list1
  | (h1::t1, h2::t2) -> h1::h2::joinLists(t1, t2)
;;

joinLists([5; 4; 3; 2], [1; 2; 3; 4; 5; 6]);;
joinLists(['a'; 'c'; 'd'], ['b']);;
joinLists([], [1; 2; 3]);;
