(* Zadanie 1 *)

let rec listSum xs =
  if xs = [] then 0
  else List.hd xs + listSum(List.tl xs);;

listSum[1;2;3;4;5;6;7;8;9] = 45;;
listSum[1;-2;3;-4;5;-6;7;-8;9] = 5;;
listSum[]=0;;

(* Zadanie 2 *)

let hardcodedSymbol = " ";;

let rec joinStrings (xs, sSymbol) =
  if xs = [] then sSymbol
  else if List.tl xs = [] then List.hd xs ^ sSymbol
  else List.hd xs ^ hardcodedSymbol ^ joinStrings(List.tl xs, sSymbol);;

joinStrings (["Jakieś"; "krótkie"; "zdanie"], ".") = "Jakieś krótkie zdanie.";;
joinStrings (["Test"], "!") = "Test!";;
joinStrings ([], ".") = ".";;
joinStrings ([], "") = "";;

(* Zadanie 3 *)

let rec greaterThanZero xs = 
  if xs = [] then false
  else if List.hd xs <= 0 then false
  else if List.tl xs = [] then true
  else greaterThanZero (List.tl xs);;

greaterThanZero[1;2;3] = true;;
greaterThanZero[1;-2;3] = false;;
greaterThanZero[-2] = false;;
greaterThanZero[] = false;;

(* Zadanie 4 *)
let rec fact x =
    if x < 0 then failwith "A negative argument was given!"
    else if x <= 1 then 1 
    else x * fact (x - 1);;

fact(0)=1;;
fact(1)=1;;
fact(2)=2;;
fact(10)=3628800;;
(* fact(-12);; *)
