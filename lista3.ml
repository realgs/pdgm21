let reverseList xs = 
  let rec reverseRec (acc, xs) =
    if xs = [] then acc
    else reverseRec ((List.hd xs)::acc, List.tl xs)
  in reverseRec ([], xs)
;;

let splitBySign xs =
  let rec splitRec (xs, negList, posOddList) =
    match xs with
      [] -> (reverseList negList, reverseList posOddList)
      |hd::tl when hd < 0 -> splitRec (tl, hd::negList, posOddList)
      |hd::tl when hd > 0 && hd mod 2 != 0 -> splitRec (tl, negList, hd::posOddList)
      |hd::tl -> splitRec (tl, negList, posOddList)
  in splitRec (xs, [], [])
;;

splitBySign [-3;-6;7;-9;13];;
splitBySign [-2;-1;0;1;2;3;4;5];;
splitBySign [-5;-4;-3];;
splitBySign [1;3;5;7];;
splitBySign [0;2;4;6;8];;

let lengthOfList xs =
  let rec lengthRec (acc, xs) =
    if xs = [] then acc
    else lengthRec(acc + 1, List.tl xs)
  in lengthRec (0, xs)
;;

lengthOfList [5;4;3;2];;
lengthOfList ["foo"];;
lengthOfList [];;

let joinLists (asList, bsList) =
  let rec joinRec (acc, switch, asList, bsList) =
    match (asList, bsList, switch) with
      ([], [], _) -> reverseList acc
      |(hd::tl, [], _) -> joinRec (hd::acc, false, tl, [])
      |([], hd::tl, _) -> joinRec (hd::acc, true, [], tl)
      |(hd::tl, bsList, false) -> joinRec (hd::acc, true, tl, bsList)
      |(asList, hd::tl, true) -> joinRec (hd::acc, false, asList, tl)
  in joinRec ([], false, asList, bsList)
;;

joinLists ([5;4;3;2], [1;2;3;4;5;6]);;
joinLists ([1;2;3;4;5], []);;
joinLists ([], [1;2;3;4;5]);;
joinLists (['a';'b';'c'], ['x';'y';'z']);;
joinLists ([], []);;