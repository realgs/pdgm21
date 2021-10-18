(* Piotr Zięba *)

(* Zadanie 1 *)
let rec sumOfList xs =
  if xs = [] then 0 
  else List.hd xs + sumOfList(List.tl xs);;

sumOfList([]) = 0;;
sumOfList([1;2;3;4;5]) = 15;;
sumOfList([0;0;1]) = 1;;

(* Zadanie 2 *)
let wordsToSentence xs = 
  let rec wordsToSentenceI (xs, sSentence) =
    if xs = [] then sSentence
    else if List.hd xs = "." then sSentence ^ "."
    else if sSentence <> "" then wordsToSentenceI(List.tl xs, sSentence ^ " " ^ List.hd xs)
    else wordsToSentenceI(List.tl xs, List.hd xs) 
  in wordsToSentenceI(xs, "");;
        
wordsToSentence(["Ala"; "ma"; "kota"; "."]) = "Ala ma kota.";;
wordsToSentence([]) = "";;
wordsToSentence(["."]) = ".";;

(* Zadanie 3 *)
let rec greaterThanZero xs =
  if xs = [] then true
  else if List.hd xs <= 0 then false
  else greaterThanZero(List.tl xs);;

greaterThanZero([1; 2; 3]) = true;;
greaterThanZero([1; 3; -7]) = false;;
greaterThanZero([0; 42; 84]) = false;;
greaterThanZero([]) = true;;


(* Zadanie 4 *)
let factorial n =
  if n < 0 then raise (Failure "Nieprawidłowy argument")
  else 
    let rec factorialI (n, a) =
      if n = 0 then a
      else factorialI(n-1, n*a) 
    in factorialI(n, 1);; 

(*factorial(-1);;*)
factorial(0) = 1;;
factorial(5) = 120;;

