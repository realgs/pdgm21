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

