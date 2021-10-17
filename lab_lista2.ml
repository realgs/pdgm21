(*Lista 2 (OCaml)*)

(*zadanie 1*)
let rec list_sum (li) = (
    if li = [] then 0
    else List.hd li + list_sum(List.tl li));;

list_sum([1; 2; 3]) = 6;;
list_sum([12; 12; 13; 14; 15]) = 66;;
list_sum([-5; -4; -3; -2; -1; 0; 1; 2; 3; 4; 5]) = 0;;
list_sum([-20]) = -20;;
list_sum([]) = 0;;

(*zadanie 2*)
let rec make_sentence (li, s) = (
    let operator = " " in

    if li = [] then s
    else if List.tl li = [] then List.hd li ^ s
    else List.hd li ^ operator ^ make_sentence(List.tl li, s));;

make_sentence(["Ala"; "ma"; "kota"], ".") = "Ala ma kota.";;
make_sentence(["Czy"; "program"; "dzia³a"; "poprawnie"], "?") = "Czy program dzia³a poprawnie?";;
make_sentence(["Aaa"], "!") = "Aaa!";;
make_sentence([], "?") = "?";;

(*zadanie 3*)
let rec greater_than_zero (li) = (
    if li = [] then false
    else if List.tl li = [] then List.hd li > 0.
    else List.hd li > 0. && greater_than_zero(List.tl li));;

greater_than_zero([1.; 2.; 3.]) = true;;
greater_than_zero([0.; 1.; 2.; 3.]) = false;;
greater_than_zero([1.2; 2.3; 3.4; 4.5]) = true;;
greater_than_zero([-5.3; 5.; 25.; 4.; 3.]) = false;;
greater_than_zero([10000.]) = true;;
greater_than_zero([0.]) = false;;
greater_than_zero([]) = false;;

(*zadanie 4*)
let rec factorial (n) = (
    if n<0 then failwith "Ujemny argument"
    else if n=0 then 1
    else n*factorial(n-1));;

factorial(0) = 1;;
factorial(1) = 1;;
factorial(2) = 2;;
factorial(5) = 120;;
factorial(10) = 3628800;;
(*factorial(-1);;*)

