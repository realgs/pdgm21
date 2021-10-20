(*Dawid Krutul*)

(*Zadanie 1*)
let rec count_sum numbers =
    if numbers = [] then 0
        else List.hd numbers + count_sum(List.tl numbers);;

count_sum([1;2;3;4]) = 10;;
count_sum([]) = 0;;
count_sum([-3;-2;-1;15]) = 9;;

(*Zadanie 2*)

let space = " ";;


let rec create_sentence (words,dot) =
    if words = [] then dot
           else List.hd words ^
                if List.tl words = [] then create_sentence(List.tl words,dot)
                else  space ^ create_sentence(List.tl words,dot);;






create_sentence(["ala";"ma";"kota"],".");;

(*Zadanie 3*)

let rec natural_numbers numbers =
    if numbers = [] then true
        else if List.hd numbers < 0 then false
            else natural_numbers(List.tl numbers);;

natural_numbers([1;2;3]) = true;;
natural_numbers([0;5;3;3;3;1;0]) = true;;
natural_numbers([-1;1;3;3;3]) = false;;


(*Zadanie 4*)

let rec factorial n =
    if n = 0 then 1
        else n * factorial(n - 1);;

factorial(0);;
factorial(1);;
factorial(2);;
factorial(3);;
factorial(4);;



