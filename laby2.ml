
(* Zadanie 1 *)
let listSum list =
  let rec listSumHelper (list, sum) =
    if list = [] then sum
    else listSumHelper ( List.tl list, sum + List.hd list)
  in listSumHelper (list, 0);;

listSum [1; 2; 3; 4; 5] = 15;;
listSum [] = 0;;
listSum [1; 123; 1; 2; 5] = 132;;

(* Zadanie 2*)

(* hardcododed separator*)
let separator = " ";;

let stringFromList list endChar =
  let rec stringFromListHelper (list, endChar, result)=
    if list = [] then result
    else if List.hd list = endChar then result ^ endChar
    else stringFromListHelper ((List.tl list), endChar, 
    result^ (
      if result = "" then ""
      else separator
    )^ (List.hd list))

  in stringFromListHelper (list, endChar, "");;

stringFromList ["Litwo,"; "Ojczyzno"; "moja"; "!"] "!" = "Litwo, Ojczyzno moja!";;
stringFromList ["test"; "dzialania"; "zadania"; "."] "." = "test dzialania zadania.";;
stringFromList [] "a" = "";;
stringFromList ["!"; "b"; "c"] "!" = "!";;
stringFromList ["W"; "B"; "!"; "c"] "!" = "W B!";;

(* Zadanie 3*)

let greaterThanZeroList list =
  let rec greaterThanHelper list = 
    if list = [] then []
    else if List.hd list > 0 then List.hd list :: greaterThanHelper (List.tl list)
    else [] 
  in 
  if list = [] then false
  else greaterThanHelper list = list;;

greaterThanZeroList [1; 2; 3] = true;;
greaterThanZeroList [] = false;;
greaterThanZeroList [1; 4; 0; 5] = false;;
greaterThanZeroList [5; -1; 2; 9] = false;;

(* Zadanie 4*)

let factorial n = 
  let rec factorialHelper n = 
    if n = 0 then 1
    else n * factorialHelper (n-1)
  in
  if n < 0 then failwith "ujemny argument"
  else factorialHelper n;;

  
factorial(5) = 120;;
factorial(0) = 1;;
factorial(10) = 3628800;;

