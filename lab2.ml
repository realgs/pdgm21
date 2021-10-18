(* Jakub Kowalczyk *)

(* Zadanie 1 *)

let rec go1(list, a) =
  if list == [] then a
  else go1(List.tl list, a + List.hd list);;

let sum list =
  go1(list, 0);;

sum([1;2;3;4;5]) == 15;;
sum([]) == 0;;
sum([-1;-2;-3;-4;-5]) == -15;;
sum([1]) == 1;;

(* Zadanie 2 *)

let kropka = ".";;
let spacja = " ";;

let rec go2(list, a) =
  if list == [] then (a ^ kropka)
  else go2(List.tl list, a ^ spacja ^ List.hd list);;

let sentence list =
  if list == [] then ""
  else go2(List.tl list, List.hd list);;

sentence(["Ala";"ma";"kota"]) = "Ala ma kota.";;
sentence(["Ala"]) = "Ala.";;
sentence(["a";"b";"c";"d";"e";"f"]) = "a b c d e f.";;
sentence([]) = "";;

(* Zadanie 3 *)

let rec areGreaterThanZero list =
  if list == [] then true
  else (if (List.hd list > 0) then areGreaterThanZero(List.tl list) else false);;
     
areGreaterThanZero([]) == true;;
areGreaterThanZero([1;2;3;4;5]) == true;;
areGreaterThanZero([0;0;0;0]) == false;;
areGreaterThanZero([-1;-2;-3;-4;-5]) == false;;

(* Zadanie 4 *)

let rec go4(n, a) =
  if n == 0 then a
  else go4(n-1, a*n);;

let factorial n =
  if n < 0 then raise(Failure"negative number")
  else go4(n, 1);;

factorial(1) == 1;;
factorial(5) == 1*2*3*4*5;;
factorial(0) == 1;;
factorial(10) == 1*2*3*4*5*6*7*8*9*10;;
