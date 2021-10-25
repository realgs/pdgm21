(*exercise 1*)
let reverseList list =

  let rec reverse (listOriginal, listReversed) = 

    if listOriginal = [] then listReversed
    else reverse (List.tl listOriginal, (List.hd listOriginal) :: listReversed)

  in reverse (list, [])
;;

let splitBySign list =

  let rec split (listOriginal, listNeg, listOddPos) =

    match listOriginal with
      [] -> (listNeg, listOddPos)
    | (head :: tail) -> 
        if head < 0 then split (tail, head :: listNeg, listOddPos)
        else if head > 0 && head mod 2 = 1 then split (tail, listNeg, head :: listOddPos)
        else split (tail, listNeg, listOddPos)
      
  in split (reverseList list, [], [])
;;

splitBySign [-1; 1; -2; 2] = ([-1; -2], [1]);;
splitBySign [] = ([], []);;
splitBySign [2; 4; 6; 8; 0] = ([], []);;


(*exercise 2*)
let rec calculateLength list =

  if list = [] then 0
  else 1 + calculateLength (List.tl list)
;;

calculateLength [] = 0;;
calculateLength [2; 2; 3; -20; -4] = 5;;
calculateLength ['a'; 'b'] = 2;;


(*exercise 3*)
let rec joinLists (list1, list2) = 

  match (list1, list2) with
    ([], []) -> []
  | ([], _) -> list2
  | (_, []) -> list1
  | (_) -> List.hd list1 :: List.hd list2 :: joinLists (List.tl list1, List.tl list2)
;;

joinLists ([5; 4; 3; 2], [1; 2; 3; 4; 5; 6]) = [5; 1; 4; 2; 3; 3; 2; 4; 5; 6];;
joinLists ([], []) = [];;
joinLists ([2; 5; -12], []) = [2; 5; -12];;
joinLists ([], ['a'; 'b'; 'c';]) = ['a'; 'b'; 'c';];;
