let rec sum list =
    if list = [] then 0 else List.hd list + sum(List.tl list);;

sum([1; 2; 3; 4; 5]) = 15;;
sum([-2; -1; 0; 5; -6; 3]) = -1;;
sum([]) = 0;;


let spacing = " ";;

let rec join(list, ending) =
    if list = [] then ""
    else
        if List.tl list = [] then List.hd list ^ ending
        else List.hd list ^ spacing ^ join(List.tl list, ending);;

join(["To"; "nie"; "tak"], "!") = "To nie tak!";;
join(["Ala"; "ma"; "kota"], "?") = "Ala ma kota?";;
join([], "some string") = "";;


let rec greater_than_zero list =
    if list = [] then false
    else
        if List.tl list = [] then List.hd list > 0
        else List.hd list > 0 && greater_than_zero(List.tl list);;

greater_than_zero([1; -5; 3; 2]) = false;;
greater_than_zero([1; 2; 5; 7; 8]) = true;;
greater_than_zero([]) = false;;


let rec factorial n =
    if n < 0 then raise (Invalid_argument "factorial argument must be greater or equal zero")
    else
        if n = 0 then 1
        else n * factorial(n - 1);;

factorial(-5);;
factorial(0) = 1;;
factorial(6) = 720;;
