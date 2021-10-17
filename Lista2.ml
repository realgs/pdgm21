(*zadanie 1*)

let rec totalSum xs =
  if xs <> [] 
  then List.hd xs + totalSum(List.tl xs) 
  else 0;;

totalSum [];;
totalSum [1; 2; 3; 4; 5; 6];;



(*zadanie 2 - program wypisuje string do pojawienia sie kropki. Jesli kropka się nie pojawi to wstawia ją / informuje o niej*)

let rec stringifyAddDot xs = 
  if xs <> [] &&  List.hd xs <> "."
      then List.hd xs  ^ " " ^ stringifyAddDot (List.tl xs)
  else ".";;

  stringifyAddDot ["This"; "is"; "a"; "correct"; "sentence"; "."];;
  stringifyAddDot ["This"; "is"; "a"; "sentence"; "."; "after"; "the"; "dot"];;
  stringifyAddDot ["Here"; "program"; "inserts"; "a"; "missing"; "dot"];;
  stringifyAddDot [];;


  let rec stringifyInformDotIsNotThere xs = 
    if xs <> [] 
     then if List.hd xs <> "."
        then List.hd xs  ^ " " ^ stringifyInformDotIsNotThere(List.tl xs)
      else "."
    else "[the dot is missing!]";;
  
    stringifyInformDotIsNotThere ["This"; "is"; "a"; "correct"; "sentence"; "."];;
    stringifyInformDotIsNotThere ["This"; "is"; "a"; "sentence"; "."; "after"; "the";"dot"];;
    stringifyInformDotIsNotThere ["Here"; "program"; "informs"; "about"; "a"; "missing"; "dot"];;
    stringifyInformDotIsNotThere [];;



(*zadanie 3*)

let rec areBiggerThanZero xs =
  if xs <> [] 
    then if List.hd xs > 0
      then areBiggerThanZero(List.tl xs) 
      else false
  else true;;

  areBiggerThanZero [];;
  areBiggerThanZero [1; 2; 3; 4; 5; 6];;
  areBiggerThanZero [1; 2; 3; 4; -5; 6];;

  (*zadanie 4*)

  let rec factorial n =
    if n = 0
      then 1 
    else n * factorial(n-1);;

  factorial(4);;
  factorial(-4);;
  factorial(0);;