(* zadanie 1 *)
let rec splitBySign xs =
  match xs with
    [] -> ([], [])
  | h :: t when h < 0 -> let (xs1, xs2) = splitBySign t in (h :: xs1, xs2)
  | h :: t when h > 0 && h mod 2 = 1 -> let (xs1, xs2) = splitBySign t in (xs1, h :: xs2)
  | h :: t -> splitBySign t;;

splitBySign[-3; -6; 7; -9; 13] = ([-3; -6; -9], [7; 13]);;
splitBySign[] = ([], []);;
splitBySign[0; 1; 2; 3; -10; 4; 6] = ([-10], [1; 3]);;
splitBySign[0; 2; 4; 6] = ([], []);;
splitBySign[-3; -1; -5] = ([-3; -1; -5], []);;
splitBySign[0; 1; 2; 2; 3] = ([], [1; 3]);;

(* zadanie 2 *)
let rec lengthOfList xs =
  if xs = [] then 0 else lengthOfList(List.tl xs) + 1;;

lengthOfList[5; 4; 3; 2] = 4;;
lengthOfList[] = 0;;
lengthOfList['1'; '2'; '3'] = 3;;
lengthOfList["dog"] = 1;;
lengthOfList[1; 2; 3; 4; 5] = 5;;

let lengthOfListTail xs =
  let rec lengthOfListIter (xs, n) =
    if xs = [] then n
    else lengthOfListIter(List.tl xs, n + 1)
  in lengthOfListIter(xs, 0);;

lengthOfListTail[5; 4; 3; 2] = 4;;
lengthOfListTail[] = 0;;
lengthOfListTail['1'; '2'; '3'] = 3;;
lengthOfListTail["dog"] = 1;;
lengthOfListTail[1; 2; 3; 4; 5] = 5;;

(* zadanie 3 *)
let rec joinLists (xs, ys) =
  match (xs, ys) with
    (h1 :: t1, h2 :: t2) -> h1 :: h2 :: joinLists(t1, t2)
  | (h1 :: t1, []) -> xs
  | ([], h2 :: t2) -> ys
  | _ -> [];;

joinLists([5; 4; 3; 2], [1; 2; 3; 4; 5; 6]) = [5; 1; 4; 2; 3; 3; 2; 4; 5; 6];;
joinLists([], []) = [];;
joinLists([], ['a'; 'b'; 'c']) = ['a'; 'b'; 'c'];;
joinLists(["dog"], []) = ["dog"];;
joinLists([1], [2; 3; 4]) = [1; 2; 3; 4];;
