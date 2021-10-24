(*lista 3 (OCaml)*)

(*zadanie 1*)
let rec splitBySign (l) = (
    match l with
      h::t -> let (l1, l2) = splitBySign(t) in
              if h<0 then (h::l1, l2)
              else if h>0 && h mod 2!=0 then (l1, h::l2)
              else (l1, l2)
    |   [] -> ([], []));;

splitBySign([-3; -6; 7; -9; 13]) = ([-3; -6; -9], [7; 13]);;
splitBySign([4; -5; 6; -7; -8; 9]) = ([-5; -7; -8], [9]);;
splitBySign([0]) = ([], []);;
splitBySign([0; 2; 4; 6; 8]) = ([], []);;
splitBySign([1; 3; 5; 7; 9]) = ([], [1; 3; 5; 7; 9]);;
splitBySign([-5; -4; -3; -2; -1; 0]) = ([-5; -4; -3; -2; -1], []);;
splitBySign([-7; 14; 13; -4; 2; 4]) = ([-7; -4], [13]);;
splitBySign([]) = ([], []);;

(*zadanie 2*)
let rec lengthOfList (l) = (
    if l = [] then 0
    else 1+lengthOfList(List.tl l));;

lengthOfList([5; 4; 3; 2]) = 4;;
lengthOfList(['a'; 'a'; 'b'; 'a']) = 4;;
lengthOfList([1.02]) = 1;;
lengthOfList([[true; false; false]; [false; true]]) = 2;;
lengthOfList([]) = 0;;

(*zadanie 3*)
let rec joinLists (l1, l2) = (
    match (l1, l2) with
      ([], l2) -> l2
    | (l1, []) -> l1
    | (h1::t1, h2::t2) -> h1::h2::joinLists(t1, t2));;                            
joinLists([5; 4; 3; 2], [1; 2; 3; 4; 5; 6]) = [5; 1; 4; 2; 3; 3; 2; 4; 5; 6];;
joinLists([-5; -4; -3; -2; -1], [5; 4; 3; 2; 1; 0]) = [-5; 5; -4; 4; -3; 3; -2; 2; -1; 1; 0];;
joinLists(['c'; 'b'; 'a'], ['b'; 'c']) = ['c'; 'b'; 'b'; 'c'; 'a'];;                                               
joinLists([], [true]) = [true];;
joinLists([], []) = [];


