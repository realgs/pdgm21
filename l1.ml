(*zadanie 1*)
let rec sum xs =
  if xs = [] then 0 else
    List.hd xs + sum (List.tl xs);;

sum [1;2;3;4;5] = 15;;
sum [] = 0;;
sum [-2;-1;0;1;2] = 0;;


(*zadanie 2*)
let space = " "

let rec makeSentence xs =
  if xs = [] then "" else
    if List.tl xs = [] then List.hd xs else
      if List.tl (List.tl xs) = [] then List.hd xs ^  makeSentence (List.tl xs) else
        List.hd xs ^ space ^ makeSentence (List.tl xs);;

makeSentence ["Ala";"ma";"kota";"."] = "Ala ma kota.";;
makeSentence [] = "";;
makeSentence ["Hi";"!"] = "Hi!";;


(*zadanie 3*)
let rec isGreaterThan0 xs =
  if xs = [] then true else
    if List.hd xs < 0 then false else
      isGreaterThan0 (List.tl xs);;

isGreaterThan0 [] = true;;
isGreaterThan0 [1;2;3;4;5;6;7] = true;;
isGreaterThan0 [1;2;3;-1;4;5] = false;;


(*zadanie 4*)
let rec factorial n =
  if n = 0 then 1 else
    n * factorial (n - 1);;

factorial 0 = 1;;
factorial 1 = 1;;
factorial 6 = 720;;
