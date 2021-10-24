(* zadanie 1 *)
let rec positiveAndOdd xs =
  if xs = [] then []
  else if List.hd xs > 0 && List.hd xs mod 2 == 1 then List.hd xs :: positiveAndOdd(List.tl xs)
  else [] @ positiveAndOdd(List.tl xs);;

let rec negative xs =
  if xs = [] then []
  else if List.hd xs < 0 then List.hd xs :: negative(List.tl xs)
  else [] @ negative(List.tl xs);;

let splitBySign xs =
  if xs = [] then ([], [])
  else (negative(xs), positiveAndOdd(xs));;
(* complexity = O(n), n = xs.length *)

(* tests *)
splitBySign [-3; -6; 7; -9; 13];;
splitBySign [-1; -2; 0; 1; 2; 3; 4];;
splitBySign [-2; 3; 1; 0; 2; 4; -1];;


(* zadanie 2 *)
let rec lengthOfList xs =
  if xs = [] then 0
  else 1 + lengthOfList(List.tl xs);;
(* complexity = O(n), n = xs.length *)

(* tests *)

lengthOfList [5; 4; 3; 2];;
lengthOfList [];;
lengthOfList [1];;
lengthOfList [1; 2; 3; 4];;
lengthOfList ['a'; 'b'; 'c'];;
lengthOfList [true; false; true];;


(* zadanie 3 *)
let rec joinLists (xs, ys) =
  if xs = [] && ys = [] then []
  else if xs = [] && ys <> [] then
    List.hd ys :: joinLists ([], List.tl ys)
  else if xs <> [] && ys = [] then
    List.hd xs :: joinLists (List.tl xs, [])
  else List.hd xs :: List.hd ys :: joinLists (List.tl xs, List.tl ys);;
(* complexity = O(n), n = max(as.length, bs.length) *)

(* tests *)

joinLists ([5; 3; 3; 2], [1; 2; 3; 4; 5; 6]);;
joinLists (['a'; 'a'; 'a'], ['b'; 'b'; 'b'; 'c']);;
joinLists (['a'; 'a'; 'a'; 'a'; 'c'], ['b'; 'b'; 'b']);;
joinLists ([], [1; 2; 3; 4; 5; 6]);;
joinLists ([1; 2; 3; 4; 5], []);;