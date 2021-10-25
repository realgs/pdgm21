(* Piotr Zięba *)

let rec reverse (xs, res) = 
  match xs with 
    []-> res
  | h::t -> reverse(t, h::res);;

(* Zadanie 1 *)
let splitBySign xs =
  let rec splitBySignI (xs, negxs, posxs) =
    match xs with
      [] -> (negxs, posxs)
    | h::t ->
        if h < 0 then splitBySignI(t, h::negxs, posxs)
        else if h > 0 && h mod 2 <> 0 then splitBySignI(t, negxs, h::posxs)
        else splitBySignI(t, negxs, posxs) 
  in splitBySignI(reverse(xs, []), [], []);;

splitBySign([-3; -6; 7; -9; 13]) = ([-3; -6; -9], [7; 13]);;
splitBySign([1; 2; 3; 0]) = ([], [1; 3]);;
splitBySign([-5; -7; -2]) = ([-5; -7; -2], []);;
splitBySign([]) = ([], []);;

(* Zadanie 2 *)
let lengthOfList xs =
  let rec lengthOfListI (xs, counter) =
    match xs with
      [] -> counter
    | h::t -> lengthOfListI(t, counter + 1)
  in lengthOfListI(xs, 0);;

lengthOfList([5; 4; 3; 2]) = 4;;
lengthOfList([-3; -6; 7; -9; 13]) = 5;;
lengthOfList(['o'; 'l'; 'a'; 'm'; 'a'; 'k'; 'o'; 't'; 'a']) = 9;;
lengthOfList([]) = 0;;

(* Zadanie 3 *)
let joinLists (xs1, xs2) =
  let rec joinListsI(xs1, xs2, res) =
    match (xs1, xs2) with
      ([], h2::t2) -> joinListsI(xs1, t2, h2::res)
    | (h1::t1, []) -> joinListsI(t1, xs2, h1::res)
    | (h1::t1, h2::t2) -> joinListsI(t1, t2, h2::h1::res)
    | ([], []) -> res 
  in reverse(joinListsI(xs1, xs2, []), []);;

joinLists([5; 4; 3; 2], [1; 2; 3; 4; 5; 6]) = [5; 1; 4; 2; 3; 3; 2; 4; 5; 6];;
joinLists([1; 3; 5; 7], [2; 4; 6; 8; 10; 12]) = [1; 2; 3; 4; 5; 6; 7; 8; 10; 12];;
joinLists([], [1; 3; 5; 7]) = [1; 3; 5; 7];;
joinLists([], []) = [];;
