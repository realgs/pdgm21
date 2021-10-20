(* Szymon Sawczuk *)

(* Zadanie 1 *)
let sum list =
    let rec sumIter(list, result) = 
        match list with
          [] -> result 
        |  _ -> sumIter(List.tl list, result + List.hd list)
    in sumIter(list, 0);;

sum([]);;
sum([2; 3; 4]);;
sum([2; -2; -1]);;
sum([3])

(* Zadanie 2 *)
let separator = " "

let listToString list =
    let rec listToStringIter(list, result) =
        match list with
          [] -> result 
        | first::[] -> listToStringIter([], result ^ first) 
        | first::second::[] -> listToStringIter(second::[], result ^ first)
        |  _ -> listToStringIter(List.tl list, result ^ List.hd list ^ separator)
    in listToStringIter(list, "");;

listToString(["Ala"; "ma"; "kota"; "!"]);;
listToString([]);;
listToString(["!"]);;
listToString(["Hello"; "!"]);;

(* Zadanie 3 *)
let isPositive list = 
    let rec isPositiveIter(list, result) =
        match list with
          [] -> result
        |  _ -> if List.hd list > 0. then isPositiveIter(List.tl list, true) else false
    in isPositiveIter(list, false);;

isPositive([]);;
isPositive([-2.; 3.; 1.5]);;
isPositive([2.3; 0.; 5.6]);;
isPositive([2.3; 9.; 8.9])

(* Zadanie 4 *)
let factorial number =
    let rec factorialIter(number, result) =
        match number with
          0 -> result
        | _ -> factorialIter(number - 1, result * number)

    in if number >= 0 then factorialIter(number, 1)
    else failwith "Not natural number!";;


factorial(0);;
factorial(5);;
factorial(8);;
