(*Sebastian Bednarski*)

(*task 1*)

let rec sum x =
  if x = [] then 0
  else List.hd x + sum(List.tl x);;

sum[1;2;3;4;5] = 15;;
sum[-1;2;3] = 4;;
sum[] = 0;;

(*task 2*)

let rec listofstr x =
  if List.tl x = [] then List.hd x^"!"
  else List.hd x^" "^listofstr(List.tl x);;

listofstr["Ala"; "ma"; "kota"] = "Ala ma kota!";;
listofstr["1"; "vs"; "2"] = "1 vs 2!";;
listofstr[""];;

(*task 3*)
let rec morezero(x) =
  if x = [] then true
  else if(List.hd x <= 0) then false
  else morezero(List.tl x);;

morezero[1;2;3;4;5];;
morezero[0];;
morezero[0;0;0;0;0];;
morezero[];;
(*task 4*)
let rec silnia n=
  if n=0 then 1 
  else if n<0 then failwith "Arggggggg!"
  else n*silnia(n-1);;

silnia 4 = 24;;
silnia 0 = 1;;
silnia (-4);;

