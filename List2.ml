(* Author: Jakub Szwedowicz *)

(*Task 1*)
let rec sum xs =
        if xs != [] then List.hd xs + sum(List.tl xs)
        else 0;;

print_string(string_of_bool(sum([1;2;3]) = 6)^"\n");;
print_string(string_of_bool(sum([0]) = 0)^"\n");;
print_string(string_of_bool(sum([]) = 0)^"\n");;


(*Task 2*)
let rec merge_strings xs =
        let sep = " " in
        if xs != [] then 
                if List.tl xs != [] then
                        if List.tl (List.tl xs) != [] then (List.hd xs)^sep^merge_strings(List.tl xs)
                        else (List.hd xs)^merge_strings(List.tl xs)
                else List.hd xs
        else "";;

print_string(string_of_bool(merge_strings(["Ala";"ma";"kota";"."]) = "Ala ma kota.")^"\n");
print_string(string_of_bool(merge_strings([""]) = "")^"\n");;
print_string(string_of_bool(merge_strings([]) = "")^"\n");;


(*Task 3*)
let rec are_positive xs =
        if xs = [] then true
        else if List.hd xs > 0 then are_positive(List.tl xs)
        else false;;


print_string(string_of_bool(are_positive([1; 2]) = true)^"\n");;
print_string(string_of_bool(are_positive([1; 2; 0]) = false)^"\n");;
print_string(string_of_bool(are_positive([]) = true)^"\n");;



(*Task 4*)
let rec factorial x =
        if x < 0 then raise (Failure "Illegal argument passed!\n")
        else if x = 0 then 1
        else x * factorial(x-1);;



print_string(string_of_bool(factorial(5) = 120)^"\n");;
print_string(string_of_bool(factorial(0) = 1)^"\n");;
print_string(string_of_bool(factorial(1) = 1)^"\n");;

