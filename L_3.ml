(*zadanie 1*)
(*let s_1 (x, y) = x;;
  let s_2 (x, y) = y;;

  let rec splitBySign xs =
  match xs with
  []   -> ([], [])
  | h::t -> 
  if h < 0 then (h::(s_1 splitBySign(t)), (s_2 splitBySign(t)))
  else if h > 0 && h mod 2 = 1 then ((s_1 splitBySign(t)), h::(s_2 splitBySign(t)))
  else ((s_1 splitBySign(t)), (s_2 splitBySign(t)))
  ;;
*)

(*zadanie 2*)
let rec lengthOfList xs =
  match xs with
      []   -> 0
    | h::t -> 1 + lengthOfList(t);;

(*zadanie 3*)
let rec joinLists (xs, ys) = 
  match (xs, ys) with
      ([], []) -> []
    | (_, [])  -> xs
    | ([], _)  -> ys
    | (h1::t1, h2::t2) -> h1::joinLists(h2::t2, t1);;

(*testy*)
(*
splitBySign [-3; -6; 7; -9; 13] = ([-3; -6; -9], [7; 13]);;
splitBySign [] = ([], []);;
splitBySign [1; 2; 3; -4; -5; 6; 7] = ([-4; -5], [1; 3; 7]);;
*)

lengthOfList [5; 4; 3; 2] = 4;;
lengthOfList [] = 0;;
lengthOfList [['a'; 'b'; 'c']; ['q'; 'w'; 'e'; 'r'; 't'; 'y']] = 2;;

joinLists ([5; 4; 3; 2], [1; 2; 3; 4; 5; 6]) = [5; 1; 4; 2; 3; 3; 2; 4; 5; 6];;
joinLists ([], []) = [];;
joinLists ([1; 2; 3], []) = [1; 2; 3];;
joinLists ([], [1; 2; 3]) = [1; 2; 3];;
joinLists ([1; 3; 5; 7], [2; 4; 6; 8]) = [1; 2; 3; 4; 5; 6; 7; 8];;

