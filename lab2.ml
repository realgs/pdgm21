(*Aleksandra Serwicka*)

(*zadanie1*)
let rec sum (xs) = (
    if xs=[] then 0
    else (List.hd xs) + sum(List.tl xs)
);;

sum([1;2;3]);;
sum([1]);;
sum([]);;


(*zadanie2*)
let space=" ";;

let rec connectWords (xs,x) = (
  if xs=[] then x
  else space ^ (List.hd xs) ^ connectWords(List.tl xs,x)
);;

connectWords(["o";"l";"a"],".");;
connectWords([],"!");;
connectWords(["ola";"ma";"kotka"],".");;

(*zadanie3*)
let rec checkPositive (xs) = (
    if xs=[] then true
    else if (List.hd xs) > 0 then checkPositive(List.tl xs)
    else false
);;

checkPositive([1;2;3]);;
checkPositive([1;2;-3]);;
checkPositive([-1;2;-3]);;
checkPositive([]);;

(*zadanie4*)
let rec factorial (x) = (
    if x<0 then failwith "Nie oblicze silni z liczby ujemnej"
    else if x=0 then 1
    else x*factorial(x-1)
);;

factorial(3);;
factorial(0);;
factorial(-1);;
