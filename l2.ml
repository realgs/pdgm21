let rec len(x)=(
    if x=[] then 0 else 1+len(List.tl x));;

(*zadanie 1*)
let rec summarize(xs)=(
    if xs=[] then 0 else List.hd xs+summarize(List.tl xs));;
summarize([1;2;3;4]);;
summarize([-5;7;0;3;34]);;
summarize([]);;

(*zadanie 2*)
let rec sentence(xs, x)=(
    if xs=[] then x else if len(xs)=1 then List.hd xs^x else List.hd xs^" "^sentence(List.tl xs,x));;
sentence(["Some";"sentence"],"?");;
sentence(["Another"; "sentence";"a";"little";"bit";"longer"],".");;
sentence([],";");;

(*zadanie 3*)
let rec greaterThanZero(xs)=(
    if xs=[] then true else if List.hd xs>0 then greaterThanZero(List.tl xs) else false);;
greaterThanZero([1;2;3;4]);;
greaterThanZero([]);;
greaterThanZero([1;-12;5]);;

(*zadanie 4*)
let rec factorial(x)=(
    if x<0 then raise(Failure"Number has to be greater or equal zero.") else if x=0 then 0 else if x=1 then 1 else x*factorial(x-1));;
factorial(-3);;
factorial(5);;
factorial(8);;


