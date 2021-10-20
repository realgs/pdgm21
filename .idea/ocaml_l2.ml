(* 1 *)
let rec sumList list =
  if list = [] then 0
  else List.hd list + sumList(List.tl list)
;;

sumList[1; 2; 3; 4; 5];;
sumList[3; 4; -3];;
sumList([]);;


(* 2 *)
let space = " ";;
let rec mergeStrings(list, char) =
  if list = [] then char
  else if List.tl list = [] then List.hd list ^ mergeStrings(List.tl list, char)
  else List.hd list ^ space ^ mergeStrings(List.tl list, char)
;;

mergeStrings(["Ala"; "ma"; "kota"], ".");;
mergeStrings(["Hello"], "!");;
mergeStrings([], "?");;


(* 3 *)
let rec positiveNumList list =
  if list = [] then false
  else if List.hd list <= 0 then false
  else if List.tl list = [] then true
  else positiveNumList(List.tl list)
;;

positiveNumList [1; 2; 3; 4; 5];;
positiveNumList [1; -2; -3; 4; 5];;
positiveNumList([]);;


(* 4 *)
let rec factorial n =
  if n = 0 then 1
  else if n>0 then n * factorial(n-1)
  else raise(Failure "Ujemny argument.")
;;

factorial(5);;
factorial(2);;
factorial(1);;
factorial(0);;
factorial(-5);;
