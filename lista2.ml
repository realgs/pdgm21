(* Szymon Bak *)

(* zadanie 1 *)
let rec sum list =
    if list = [] then 0
    else List.hd list + sum(List.tl list);;

sum([1; 2; 3; 4; 5]) = 15;;
sum([]) = 0;;
sum([-1; 1; -2; 2; -3; 3]) = 0;;

(* zadanie 2 *)
let separator = " "
let rec createString(list, endChar) =
    if list = [] then endChar
    else if List.tl list = [] then List.hd list ^ createString(List.tl list, endChar)
    else List.hd list ^ separator ^ createString(List.tl list, endChar);;

createString(["Hello"; "World"], "!") = "Hello World!";;
createString([], "?") = "?";;
createString(["Ala"; "ma"; "kota"], ".") = "Ala ma kota.";;

(* zadanie 3 *)
let areAllNumbersPositive list =
    let rec areAllNumbersPositiveHelper list = 
        if list = [] then true
        else if List.hd list > 0 then areAllNumbersPositiveHelper(List.tl list)
        else false
    in if list = [] then false (* empty list returns false *)
    else areAllNumbersPositiveHelper list;;

areAllNumbersPositive([1; 2; 3; 4; 5]) = true;;
areAllNumbersPositive([5; 4; 3; 2; 1; 0; 2]) = false;;
areAllNumbersPositive([]) = false;;

(* zadanie 4 *)
let calculateFactorial number = 
    if number < 0 then failwith "Invalid Argument"
    else let rec calculateFactorialAccum(number, accumulator) =
        if number <= 1 then accumulator
        else calculateFactorialAccum(number - 1, accumulator * number)
    in calculateFactorialAccum(number, 1);;

calculateFactorial(5) = 120;;
(* calculateFactorial(-5);; Exception *)
calculateFactorial(0) = 1;;
calculateFactorial(4) = 24;;
