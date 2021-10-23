(*task 1*)
let splitBySign numList =
  let rec split (numList, negativeNumList, positiveOddNumList) =
    if numList = [] then [negativeNumList, positiveOddNumList]
    else if List.hd numList < 0 then split(List.tl numList, negativeNumList @ [List.hd numList], positiveOddNumList)
    else if List.hd numList mod 2 = 1 then split(List.tl numList, negativeNumList, positiveOddNumList @ [List.hd numList])
    else split(List.tl numList, negativeNumList, positiveOddNumList) in
  split(numList, [], []);;

splitBySign([-3; -6; 7; -9; 13]);;
splitBySign([]);;
splitBySign([1; 2; -3; -4; 5; 6; -7; -8; 9; 10]);;


(*task 2*)
let rec lengthOfList list =
  if list = [] then 0 else 1 + lengthOfList(List.tl list);;

lengthOfList([5; 4; 3; 2]);;
lengthOfList(["c"; "b"; "a"]);;
lengthOfList([]);;

(*task 3*)
let rec joinLists (list1, list2) =
  match (list1, list2) with
   |([], _) -> list2
   |(_, []) -> list1
   | _ -> [List.hd list1] @ [List.hd list2] @ joinLists(List.tl list1, List.tl list2);;

joinLists([5; 4; 3; 2], [1; 2; 3; 4; 5; 6]);;
joinLists(["Ala"; "kota"], ["ma"]);;
joinLists([], []);;
