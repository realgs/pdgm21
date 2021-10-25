(* 1 *)
let rec reverse (xs: int list) (result: int list) =
  match xs with
    [] -> result
    | (h::t) -> reverse t (h :: result);;

let splitBySign (xs: int list) =
  let rec split(xs: int list) (neg: int list) (pos: int list) =
    match xs with
      [] -> (reverse neg [], reverse pos [])
      | (h::t) when  h < 0 -> split t (h :: neg) pos
      | (h::t) when h mod 2 <> 0 -> split t neg (h :: pos)
      | (h::t) -> split t neg pos in
  split xs [] [];;

  splitBySign [-3;-6;7;-9;13] = ([-3;-6;-9],[7;13]);;
  splitBySign [] = ([], []);;
  splitBySign [0; 2; 4; 6; 8] = ([], []);;

(* 2 *)
let rec lengthOfList (xs : 'a list) =
 	if xs = [] then 0
  else 1 + lengthOfList(List.tl xs);;
 
 lengthOfList [1; 2; 3; 4] = 4;;
 lengthOfList [] = 0;;
 lengthOfList [[[]]; [[]]; [[]]] = 3;;

(* 3 *)
let rec joinLists xs ys = 
  match xs, ys with
     ([], _) -> ys
     | ( _, []) -> xs
     | (hx::tx, hy::ty) -> hx :: hy :: joinLists tx ty;;

joinLists [5;4;3;2] [1;2;3;4;5;6] = [5;1;4;2;3;3;2;4;5;6];;
joinLists [] [] = [];;
joinLists [] ['a'; 'b'; 'c'] = ['a'; 'b'; 'c'];;
joinLists [1; 1; 1; 1; 1; 1; 1] [2; 2; 2; 2; 2; 2] = [1; 2; 1; 2; 1; 2; 1; 2; 1; 2; 1; 2; 1];;

