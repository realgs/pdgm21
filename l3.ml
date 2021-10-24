(* Paulina Drzazga *)
(*zadanie 1*)

let rec splitBySign xs =
  match xs with
    [] -> ([], [])
    | h::t -> let (neg, pozOdd) = splitBySign t in
      if h < 0 then (h :: neg, pozOdd)
      else if h mod 2 != 0 then (neg, h :: pozOdd)
      else (neg, pozOdd)
;;

splitBySign [(-3);(-6);7;(-9);13]=([-3;-6;-9],[7;13]);;
splitBySign []=([],[]);;
splitBySign [1;2;3;4;5;6]=([],[1;3;5]);;

(*
Złożoność obliczeniowa: O(n) - n - długość listy
Złożoność pamięciowa: O(n) - n - długość listy
*)

(*zadanie 2*)

let rec lengthOfList xs=
  if xs=[] then 0
  else 1+lengthOfList (List.tl xs)
;;

lengthOfList ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h']=8;;
lengthOfList [5; 1; 4; 3; 2]=5;;
lengthOfList []=0;;

(*
Złożoność obliczeniowa: O(n) - n - długość listy
Złożoność pamięciowa: O(n) - n - długość listy
*)

(*zadanie 3*)

let rec joinLists xs ys=
   match (xs,ys) with
      ([], _) -> ys
      | (_, []) -> xs
      | (hx::tx, hy::ty) -> hx::(hy::(joinLists tx ty))
;;



joinLists [5;4;3;2] [1;2;3;4;5;6]=[5; 1; 4; 2; 3; 3; 2; 4; 5; 6];;
joinLists [] [1;2;3;4;5;6]=[1; 2; 3; 4; 5; 6];;
joinLists [5;4;3;2] [1]=[5; 1; 4; 3; 2];;
joinLists ['a';'c';'e';'g'] ['b';'d';'f';'h']=['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'];;
joinLists [] []=[];;

(*
Złożoność obliczeniowa: O(n) - n - długość krótszej listy
Złożoność pamięciowa: O(n) - n - długość krótszej listy
*)
