
let reverseList list =
  let rec reverseListIter (list, listReverse) =
    if list = [] then listReverse else
      reverseListIter(List.tl list, List.hd list::listReverse)
  in reverseListIter(list, []) ;;

(*zadanie 1*)

let splitBySign intList =
  let rec splitBySignIter (intList, negativeValues, positiveOddValues) =
    match intList with
      [] -> (negativeValues, positiveOddValues)
    | head::tail when head < 0 -> splitBySignIter(tail, head::negativeValues, positiveOddValues)
    | head::tail when head mod 2 = 1 -> splitBySignIter(tail, negativeValues, head::positiveOddValues)
    | _ -> splitBySignIter(List.tl intList, negativeValues, positiveOddValues)
  in splitBySignIter(reverseList(intList), [], []) ;;

splitBySign([-3; -6; 7; -9; 13]) = ([-3; -6; -9], [7; 13]) ;;
splitBySign([]) = ([], []) ;;
splitBySign([2; 4; 6]) = ([], []) ;;
splitBySign([-1; 2; -6; 4]) = ([-1; -6], []) ;;
splitBySign([2; 3; 5; 10]) = ([], [3; 5]) ;;

(*zadanie 2*)

let lengthOfList list =
  let rec lengthOfListIter (list, length) =
    if list = [] then length else
      lengthOfListIter(List.tl list, length + 1)
  in lengthOfListIter(list, 0) ;;

lengthOfList [5; 4; 3; 2] = 4 ;;
lengthOfList [] = 0 ;;
lengthOfList ["ala"; "ma"; "kota"] = 3 ;;
lengthOfList ['x'] = 1 ;;

(*zadanie 3*)

let joinLists (firstList, secondList) =
  let rec joinListsIter (firstList, secondList, finalList) =
    match (firstList, secondList) with
      (head1::tail1, head2::tail2) -> joinListsIter(tail1, tail2, head2::head1::finalList)
    | ([], head::tail) -> joinListsIter([], tail, head::finalList)
    | (head::tail, []) -> joinListsIter(tail, [], head::finalList)
    | ([],[]) -> finalList
  in reverseList(joinListsIter (firstList, secondList, [])) ;;

joinLists([5; 4; 3; 2;], [1; 2; 3; 4; 5; 6]) = [5; 1; 4; 2; 3; 3; 2; 4; 5; 6] ;;
joinLists([], []) = [] ;;
joinLists([], ['x']) = ['x'] ;;
joinLists(['x'], []) = ['x'] ;;
joinLists(["Adam"; "Bartek"], ["Ania"; "Beata"]) = ["Adam"; "Ania"; "Bartek"; "Beata"] ;;
