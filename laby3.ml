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


(* zadanie 2 *)
let rec lengthOfList xs =
  if xs = [] then 0
  else 1 + lengthOfList(List.tl xs);;
(* complexity = O(n), n = xs.length *)


(* zadanie 3 *)
let rec joinLists (xs, ys) =
  if xs = [] && ys = [] then []
  else if xs = [] && ys <> [] then
    List.hd ys :: joinLists ([], List.tl ys)
  else if xs <> [] && ys = [] then
    List.hd xs :: joinLists (List.tl xs, [])
  else List.hd xs :: List.hd ys :: joinLists (List.tl xs, List.tl ys);;
(* complexity = O(n), n = max(as.length, bs.length) *)
