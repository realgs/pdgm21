(* zadanie 1 *)

let sumInts xs =
  let rec sumIntsRec (xs, acc) =
    if xs = [] then acc
    else sumIntsRec (List.tl xs, acc + List.hd xs)
  in sumIntsRec (xs, 0)
;;

sumInts [0;1;2;3;4;5;6] = 21;;
sumInts [] = 0;;
sumInts [-1;1;0;0;5;5] = 10;;

(* zadanie 2 *)

let concatStrings (xs, endChar) =
  let rec concatStringsRec (xs, acc) =
    if xs = [] then acc ^ endChar
    else if List.tl xs = [] then acc ^ List.hd xs ^ endChar
    else concatStringsRec (List.tl xs, acc ^ List.hd xs ^ " ")
  in concatStringsRec (xs, "")
;;

concatStrings (["Ala"; "ma"; "kota"], "!") = "Ala ma kota!";;
concatStrings ([], ".") = ".";;
concatStrings (["kot"], "y") = "koty";;

(* zadanie 3 *)

let rec isAllPositive xs =
  if xs = [] then true
  else if List.hd xs > 0 then isAllPositive (List.tl xs)
  else false
;;

isAllPositive [1;2;3;4;5] = true;;
isAllPositive [1;2;3;0;4;5] = false;;
isAllPositive [-1] = false;;
isAllPositive [1] = true;;
isAllPositive [] = true;;

(* zadanie 4 *)

let factorial n =
  let rec factorialRec (n, acc) =
    if n = 0 then acc
    else if n > 0 then factorialRec(n - 1, acc * n)
    else raise (Failure "ujemny argument")
  in factorialRec (n, 1)
;;

factorial(0) = 1;;
factorial(1) = 1;;
factorial(2) = 2;;
factorial(3) = 6;;
factorial(10) = 3628800;;
