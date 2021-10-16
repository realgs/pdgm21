(*zadanie 1*)

let rec listSum xs =
  if xs = [] then 0
  else List.hd xs + listSum (List.tl xs) ;;

listSum [] = 0 ;;
listSum [2] = 2 ;;
listSum [-2; 3; -4; 1] = -2 ;;

(*zadanie 2*)

let rec sentenceFromList xs =
  if xs = [] then ""
  else if List.tl xs = [] then List.hd xs
  else List.hd xs ^ " " ^ sentenceFromList (List.tl xs) ;;

sentenceFromList [] = "" ;;
sentenceFromList ["!"] = "!" ;;
sentenceFromList ["zadanie"; "drugie"; "."] = "zadanie drugie ." ;;

(*zadanie 3*)

let rec listGreaterThanZero xs =
  if xs = [] then failwith("Empty list !")
  else if List.hd xs <= 0 then false
  else if List.tl xs <> [] then listGreaterThanZero (List.tl xs)
  else true ;;

(*listGreaterThanZero [] ;;*)
listGreaterThanZero [1] = true ;;
listGreaterThanZero [-1] = false ;;
listGreaterThanZero [1; 2; 0] = false ;;

(*zadanie 4*)

let rec factorial n =
  if n < 0 then failwith("Negative argument !")
  else if n = 0 then 1
  else n * factorial (n-1) ;;

(*factorial (-1) ;;*)
factorial 0 = 1 ;;
factorial 1 = 1 ;;
factorial 3 = 6 ;;
