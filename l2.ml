(*1*)
let rec sumOfElements (xs : int list)  =
	if xs = [] then 0
	else List.hd xs + sumOfElements(List.tl xs);;
	
sumOfElements [1;2;3;4;5;6;7;8;9] = 45;;
sumOfElements [] = 0;;
sumOfElements [1;-1;2;-2;3;-3;4;-4] = 0;;

(*2*)
let rec sentence (xs : string list) (ch : char) : string =
	if xs = [] then Char.escaped ch
	else (List.hd xs)^" "^sentence(List.tl xs) ch;;

sentence ["Ala"; "ma"; "kota"] '!' = "Ala ma kota !";;
sentence [] '?' = "?";; 	
sentence [""; ""; ""; ""; ""] '.' = "     .";;

(*3*)
let rec positive (xs : int list) = 
	if xs = [] then true
	else 
		if List.hd xs < 0 then false
		else positive(List.tl xs);;

positive [] = true;;
positive [1;2;3] = true;;
positive [1;2;(-1)] = false;;

(*4*)
 let rec factorial n = 
  	if n=0 then 1
	else if n>0 then n*factorial(n-1)
	else raise(Failure "Nonnegative number expected!");;   

factorial 5 = 120;;
factorial 0 = 1;;
(*factorial (-1)*)
