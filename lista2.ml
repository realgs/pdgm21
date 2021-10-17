(*zadanie 1*)

let rec listSum intList =
  if intList = [] then 0
  else List.hd intList + listSum (List.tl intList) ;;

listSum [] = 0 ;;
listSum [2] = 2 ;;
listSum [-2; 3; -4; 1] = -2 ;;

(*zadanie 2*)

let rec sentenceFromList (stringList, endString)  =
  if stringList = [] then endString
  else List.hd stringList ^ " " ^ sentenceFromList (List.tl stringList, endString) ;;

sentenceFromList ([], "") = "" ;;
sentenceFromList ([], "!") = "!" ;;
sentenceFromList (["zadanie"; "drugie"], ".") = "zadanie drugie ." ;;

(*zadanie 3*)

let rec listGreaterThanZero intList =
  if intList = [] then failwith("Empty list !")
  else if List.hd intList <= 0 then false
  else if List.tl intList <> [] then listGreaterThanZero (List.tl intList)
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
