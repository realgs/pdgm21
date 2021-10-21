(*Filip Brzeziak 260400*)

let space=" ";;
let dot= ".";;

(*Zadanie 1*)
let rec sumOfInts(list) =
  if list=[] then 0
  else List.hd list + sumOfInts (List.tl list)
;;

sumOfInts([-1;5;-5])=(-1);;
sumOfInts([0;2;3])=5;;
sumOfInts([])=0;;

(*Zadanie 2*)
let rec listOfStrings(list)=
    if list=[] then ""
    else if List.hd list="$" then dot
    else
        List.hd list ^ space ^ listOfStrings(List.tl list)
;;

listOfStrings(["Filip";"ma";"psa";"$"])= "Filip ma psa.";;
listOfStrings(["Filip";"ma";"psa";"$"])= "Filip ma psa.";;
listOfStrings(["Filip";"ma";"psa";"$"])= "Filip ma psa.";;

(*Zadanie 3*)
let rec greaterThanZero(list) =
    if list = [] then false
    else if List.hd list <= 0 then false
    else
        if List.tl list = [] then true
        else greaterThanZero(List.tl list)
;;

greaterThanZero([-2;4;1])=false;;
greaterThanZero([1;4;1])=true;;
greaterThanZero([])=false;;

(*Zadanie 4*)
let rec factorial(n)=
    if n<0 then failwith("Negative number!")
    else if n=0 then 1 else n*factorial(n-1)
;;
factorial(4)=24;;
factorial(0)=1;;
(*factorial(-1);;*)
