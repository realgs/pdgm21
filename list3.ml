(* Maciej Olejnik *)

(* first task *)

let splitBySign list =
  let rec splitBySignIter (list, negativeList, positiveList) =
    if list = [] then
      (negativeList, positiveList)
    else if List.hd list < 0 then
      splitBySignIter(List.tl list, negativeList @ [List.hd list], positiveList)
    else if List.hd list mod 2 = 1 then
      splitBySignIter(List.tl list, negativeList, positiveList @ [List.hd list])
    else
      splitBySignIter(List.tl list, negativeList, positiveList)
  in splitBySignIter(list, [], []);;

splitBySign([-3; -6; 7; -9; 13]) = ([-3; -6; -9], [7; 13]);;
splitBySign([]) = ([], []);;
splitBySign([2; 4; 6; 8; -1]) = ([-1], []);;

(* second task *)

let lengthOfList list =
  let rec lengthOfListIter (list, lengthAccum) =
    if list = [] then
      lengthAccum
    else
      lengthOfListIter(List.tl list, lengthAccum + 1)
  in lengthOfListIter(list, 0);;

lengthOfList([5; 4; 3; 2]) = 4;;
lengthOfList(["string"]) = 1;;
lengthOfList([]) = 0;;

(* third task *)

let joinLists (firstList, secondList) =
  let rec joinListsIter (firstList, secondList, index, resultList) =
    if index mod 2 = 0 then
      if firstList = [] then
        if secondList = [] then
          resultList
        else
          joinListsIter(firstList, List.tl secondList, index + 1, resultList @ [List.hd secondList])
      else
        joinListsIter(List.tl firstList, secondList, index + 1, resultList @ [List.hd firstList])
    else if secondList = [] then
      if firstList = [] then
        resultList
      else
        joinListsIter(List.tl firstList, secondList, index + 1, resultList @ [List.hd firstList])
    else
      joinListsIter(firstList, List.tl secondList, index + 1, resultList @ [List.hd secondList])
  in joinListsIter(firstList, secondList, 0, []);;

joinLists([5; 4; 3; 2], [1; 2; 3; 4; 5; 6]) = [5; 1; 4; 2; 3; 3; 2; 4; 5; 6];;
joinLists([], []) = [];;
joinLists([2; 3; 4], []) = [2; 3; 4];;

(* #use "list3.ml";; *)
