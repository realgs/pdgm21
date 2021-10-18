(* Gracjan Pasik *)

(* Zadanie 1 *)
let rec sum_list list =
    match list with
    | head::tail -> head + sum_list tail
    | [] -> 0
    ;;

sum_list [1;2;3;19];;
sum_list [];;
sum_list [-1];;

(* Zadanie 2 *)
let rec list_to_one_string list_of_words ending_character =
    match list_of_words with
    | [] -> ending_character
    | head::tail -> match tail with
                    | [] -> head ^ ending_character
                    | head2::tail2 -> head ^ " " ^ list_to_one_string tail ending_character
                    ;;

list_to_one_string ["Ala";"ma";"kota"] ".";;
list_to_one_string [] "?";;
list_to_one_string ["Hello"] "!";;


(* Zadanie 3 *)
let rec are_numbers_greater_than_0 numbers =
    match numbers with
    | [] -> true
    | head::tail -> if (head > 0) then are_numbers_greater_than_0 tail
                    else false    
    ;;

are_numbers_greater_than_0 [1;2;3;4;5];;
are_numbers_greater_than_0 [-5;-6];;
are_numbers_greater_than_0 [];;
are_numbers_greater_than_0 [0;0;0];;


(* Zadanie 4 
    założenie: liczba jest >= 0, inaczej silnia nie jest zdefiniowana *)
let factorial number =
    let rec accumulating number value =
        if number = 0 then value
        else accumulating (number-1) (number*value)
    in accumulating number 1;;

factorial 5;;
factorial 1;;
factorial 0;;
factorial 8;;

