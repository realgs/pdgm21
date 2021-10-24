(* task 1 *)

let reverse =
  let rec rev newList list =
    match list with
      [] -> newList
    | h::t -> rev(h::newList) t in
  fun list -> rev [] list;;


let splitBySign(list) =
  let rec split (f, orginalList, newList) = 
      match orginalList with
        [] -> reverse(newList)
      | h::t -> if f h then split(f, t, h::newList) else split(f, t, newList) in
  (split(x -> x < 0, list, []), split(x -> x > 0 && x%2=1, list, []));;

splitBySign([-3; -6; 7; -9; 13]) = ([-3; -6; -9], [7; 13]);;
splitBySign([1; -2; 8; -1; 9; 3; -4; 6]) = ([-2; -1; -4], [1; 9; 3]);;
splitBySign([]) = ([], []);;


(* task 2 *)

let rec listLength(list) =
  if list = [] then 0 else 1 + listLength(List.tl list);;

listLength([]) = 0;;
listLength([2]) = 1;;
listLength([-8; 2; 1]) = 3;;


(* task 3 *)

let rec joinLists (list1, list2) =
  match (list1, list2) with
    ([], []) -> []
  | (h::t, []) -> list1
  | ([], h::t) -> list2
  | (h1::t1, h2::t2) -> h1 :: h2 :: joinLists(t1, t2);;

joinLists([5; 4; 3; 2], [1; 2; 3; 4; 5; 6]) = [5; 1; 4; 2; 3; 3; 2; 4; 5; 6];;
joinLists([], []) = [];;
joinLists([], [1; 2]) = [1; 2];;
joinLists([1; 6], []) = [1; 6];;
joinLists(['a'; 'b'], ['c']) = ['a'; 'c'; 'b'];;

             
