(*Zadanie 1*)
let rec sum xs =
  if xs = [] then 0
  else List.hd xs + sum(List.tl xs);;

sum [1;2;3;11;25] = 42;;
sum [~-5; 7; 8; 9] = 19;;
sum [] = 0;;

(*Zadanie 2*)
let space = " ";;

let rec sentence (xs, x) =
  if xs = [] then x
  else space ^ List.hd xs ^ sentence(List.tl xs, x);;

sentence(["Polacz"; "te"; "slowa"], ".");;
sentence(["Jestem"; "zdaniem"; "wykrzyknikowym"], "!");;
sentence([], "?");;

(*Zadanie 3*)
let rec isPositive xs =
  if xs = [] then true
  else if List.hd xs > 0 then isPositive(List.tl xs)
  else false;;

isPositive [2;4;5;6] = true;;
isPositive [0;5;8;6;5;8] = false;;
isPositive [~-1;5;9;11;55] = false;;
isPositive [] = true;;

(*Zadanie 4*)
let rec factorial x =
  if x < 0 then failwith "Ujemny argument!"
  else if x = 0 then 1
  else x*factorial(x-1);;

factorial 6 = 720;;
factorial 12 = 479001600;;
factorial 0 = 1;;
(*factorial(-5)*)
