let rec sumaElementow xs = 
	if xs=[] then 0
	else List.hd xs + sumaElementow (List.tl xs);;


let rec utworzZdanie (xs, znak) = 
	if xs=[] then znak
	else if List.tl xs = [] then (List.hd xs) ^ znak
	else (List.hd xs) ^ " " ^ utworzZdanie (List.tl xs, znak);;


let rec czyDodatnie xs = 
	if xs=[] then true
	else if List.hd xs <= 0 then false 
	else czyDodatnie(List.tl xs);;


let rec silnia x = 
	if x<0 then raise (Failure "ujemny argument")
	else if x=0 then 1
	else x*silnia(x-1);;
	



(* zadanie 1 *)
print_string "zadanie 1";;
sumaElementow [-1;2;3;5] = 9;;
sumaElementow [] = 0;;
sumaElementow [1] = 1;;


(* zadanie 2 *)
print_string  "zadanie 2";;
utworzZdanie(["ala"; "ma"; "kota"], ".") = "ala ma kota.";;
utworzZdanie(["ok"], "!") = "ok!";;
utworzZdanie([], "?") = "?";;

(* zadanie 3 *)
print_string "zadanie 3";;
czyDodatnie [1;2;3;5] = true;;
czyDodatnie [1;0;1] = false;;
czyDodatnie [-1] = false;;
czyDodatnie [1] = true;;
czyDodatnie [] = true;;

(* zadanie 4 *)
print_string "zadanie 4";;
silnia 0  = 1;;
silnia 1 = 1;;
silnia 5 = 120;;
(*silnia -1;;*)  (* wyrzuca wyjatek *)
