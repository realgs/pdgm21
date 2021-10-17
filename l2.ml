(*1*)
let rec listSum list = 
  if list=[] then 0
  else List.hd list + listSum(List.tl list)
;;

listSum([1; 2; 3]) = 6;;
listSum([1]) = 1;;
listSum([]) = 0;;

(*2*)
let rec listLength list =
  if list==[] then 0
  else 1 + listLength(List.tl list)
;;

let rec createSentence list =
  if listLength(list)=0 then ""
  else if listLength(list)=1 then List.hd list
  else if List.tl list=["."] || List.tl list=["!"] || List.tl list=["?"] then List.hd list ^ List.hd (List.tl list)
  else List.hd list ^ " " ^ createSentence(List.tl list)
;;

createSentence(["Ala"; "ma"; "kota"; "."]) = "Ala ma kota.";;
createSentence(["Ala"]) = "Ala";;
createSentence(["Ala"; "ma"]) = "Ala ma";;
createSentence(["."]) = ".";;
createSentence([]) = "";;

(*3*)
let rec positiveCheck list =
  if list=[] then true
  else if List.hd list<=0 then false
  else positiveCheck(List.tl list)
;;

positiveCheck([1; 2; 3]) = true;;
positiveCheck([1; 2; -3]) = false;;
positiveCheck([]) = true;;

let rec strictPositiveCheck list =
  listLength(list) <> 0 && positiveCheck(list)
;;

strictPositiveCheck([1; 2; 3]) = true;;
strictPositiveCheck([1; 2; -3]) = false;;
strictPositiveCheck([]) = false;;

(*4*)
let rec factorial n =
  if n=0 then 1
  else n*factorial(n-1)
;;

factorial(0) = 1;;
factorial(1) = 1;;
factorial(3) = 6;;
