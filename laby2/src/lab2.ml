(*zadanie1*)
let rec sumList xs =
    if xs = [] then 0
    else List.hd xs + sumList(List.tl xs);;

sumList([1;2;3;4]);;
sumList([]);;
sumList([2]);;

(*zadanie2*)
let rec sentenceList xs =
    if xs = [] then ""
    else List.hd xs ^ " " ^ sentenceList(List.tl xs);;

sentenceList(["Ala"; "ma"; "kota"; "."]);;
sentenceList(["?"]);;
sentenceList([]);;

(*zadanie3*)
let rec biggerThanZero xs =
    if xs = [] then true
    else if List.hd xs <= 0 then false
    else biggerThanZero(List.tl xs);;

biggerThanZero([1;2;3;4;5]);;
biggerThanZero([0;-1;2;3]);;

(*zadanie4*)
let rec factorial x =
    if x = 0 then 1
    else x*factorial(x-1);;

factorial(0);;
factorial(3);;
factorial(10);;
