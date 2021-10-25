let splitBySign xs =
  let rec splitHelper(xs, neg_list, pos_list) =
    if xs = [] then [neg_list, positiveOddNumList]
    else if List.hd xs < 0 then splitHelper(List.tl xs, neg_list @ [List.hd xs], pos_list)
    else if List.hd xs mod 2 = 1 then splitHelper(List.tl xs, neg_list, pos_list @ [List.hd xs])
    else splitHelper(List.tl xs, neg_list, pos_list) in
    splitHelper(xs, [], []);;

splitBySign([-3; -6; 7; -9; 13]);;
splitBySign([]);;


let rec lengthOfList list =
  if list = [] then 0 else 1 + lengthOfList(List.tl list);;

lengthOfList([0; 0; 0; 0]);;
lengthOfList([]);;

(*task 3*)
let rec joinLists (list1, list2) =
  match (list1, list2) with
    |([], _) -> list2
    |(_, []) -> list1
    |(_, _) -> [List.hd list1] @ [List.hd list2] @ joinLists(List.tl list1, List.tl list2);;

joinLists([1; 3; 5; 7], [2; 4; 6; 8]);;
joinLists([], []);;
