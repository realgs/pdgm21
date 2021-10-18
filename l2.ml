(*zadanie 1*)
let rec suma xs =
  if xs=[] then 0
  else List.hd xs + suma(List.tl xs);;

suma[] = 0;;
suma[1;2;3;4;5] = 15;;
suma[-5;0;7;-3] = -1;;

(*zadanie 2*)
let rec zdanie xs =
  if xs=[] then ""
  else if List.hd xs = "." then List.hd xs
  else if List.tl xs = ["."] then List.hd xs ^ "."
  else List.hd xs ^ " " ^ zdanie(List.tl xs);;

zdanie[] = "";;
zdanie["Ala";"ma";"kota";"."] = "Ala ma kota.";;
zdanie["xyz";"."] = "xyz.";;
zdanie["."] = ".";;

(*zadanie 3*)
let rec czyDodatnie xs =
  if xs=[] then true
  else if List.hd xs <= 0 then false
  else czyDodatnie(List.tl xs);;

czyDodatnie[1;2;3];;
czyDodatnie[];;
czyDodatnie[-1;2;-3] = false;;
czyDodatnie[5;5;0] = false;;

(*zadanie 4*)
let rec silnia n =
  if n=0 then 1
  else if n>0 then n*silnia(n-1)
  else failwith("ujemny argument");;

silnia(0) = 1;;
silnia(1) = 1;;
silnia(5) = 120;;
(*silnia(-1);;*)

