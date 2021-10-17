(* zadanie 1 *)

let rec addListElements list =
  if list = [] then 0
  else List.hd list + addListElements(List.tl list);;

addListElements([]) = 0;;
addListElements([1]) = 1;;
addListElements([-1; 1]) = 0;;
addListElements([1; 2; 3; 4; 5; 6]) = 21;;


(* zadanie 2 *)

let charBetweenWords = " ";;

let rec createSentence(list, lastCharacter) =
  if list = [] then lastCharacter
  else if List.tl list != [] then List.hd list ^ charBetweenWords ^ createSentence(List.tl list, lastCharacter)
  else List.hd list ^ createSentence(List.tl list, lastCharacter);;

createSentence([], ".") = ".";;
createSentence(["Ala"; "ma"; "kota"], ".") = "Ala ma kota.";;
createSentence(["Czy"; "Ala"; "ma"; "kota"], "?") = "Czy Ala ma kota?";;


(* zadanie 3 *)

let rec findIfNumbersGreaterThanZero list =
  if list = [] then true
  else if List.hd list > 0 then findIfNumbersGreaterThanZero(List.tl list)
  else false;;

findIfNumbersGreaterThanZero([]) = true;;
findIfNumbersGreaterThanZero([1; 5; -1]) = false;;
findIfNumbersGreaterThanZero([1]) = true;;
findIfNumbersGreaterThanZero([-2]) = false;;


(* zadanie 4 *)

let rec factorialHelper(number, result) =
  if number == 0 then result
  else factorialHelper(number - 1, result * number);;

let rec factorial number = factorialHelper(number, 1);;

factorial(0) = 1;;
factorial(1) = 1;;
factorial(2) = 2;;
factorial(8) = 40320;;
factorial(10) = 3628800;;
