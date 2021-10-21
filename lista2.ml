(*Magdalena So�tysiak*)

(*zadanie 1*)
let rec sum xs =
  if xs = [] then 0
  else List.hd xs + sum(List.tl xs);;

sum[1;2;3] = 6;;
sum[] = 0;;
sum[-2;-4;6;-8;-9] = -17;;

(*zadanie 2*)
let tmpString = " ";;
let rec stringSum (xs,x) =
  if xs = [] then x
  else List.hd xs ^ tmpString ^ stringSum(List.tl xs, x);;

stringSum(["Ola";"ma";"kota"], "!");;
stringSum([],"?");;
stringSum(["Ala";"Ola";"i";"Ania"], "!");;


(*zadanie 3*)
let rec checkNumber xs =
  if xs = [] then true
  else
    if List.hd xs > 0 then checkNumber(List.tl xs)
    else false;;

checkNumber[1;2;3;4;5];;
checkNumber[-2;3;-5;7];;
checkNumber[];;

(*zadanie 4*)

let rec factorial x =
  if x < 0 then failwith("ujemny argument")else
  if x = 0 then 1
  else x * factorial(x-1);;

factorial(4);;
factorial(-5);;
factorial(12);;




