(*Lobocka Paulina*)


(* 1 *)

let rec sum list =
    if list = [] then 0
    else List.hd list + sum(List.tl list);;

sum([1; 2; 5]) = 8;;
sum([]) = 0;;
sum([-2; 13; 0]) = 11;;


(* 2 *)

let rec append (list, char) =
    if list = [] then char
    else if List.tl list = [] then List.hd list ^ char
    else List.hd list ^ " " ^ append(List.tl list, char);;

append(["testing"; "my"; "code"], "!") = "testing my code!";;
append([""], ".") = ".";;
append([], ".") = ".";;


(* 3 *)

let rec arePositive list =
    if list = [] then true
    else if List.hd list > 0 then arePositive(List.tl list)
    else false;;

arePositive([1; 12; 1654; 7]) = true;;
arePositive([-2; -3; 0]) = false;;
arePositive([]) = true;;


(* 4 *)

let rec factorial nr =
    if nr < 0 then raise (Failure "negative argument")
    else if nr < 2 then 1
    else nr * factorial(nr - 1);;

factorial(4) = 24;;
factorial(0) = 1;;
factorial(10) = 3628800;;
