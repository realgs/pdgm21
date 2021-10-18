
(*zadanie 1*)
let rec suma xs =
  if xs != [] then List.hd xs + suma(List.tl xs) else 0;;

suma [1;2;3;4];;
suma [3];;
suma [];;
suma [1;2;-3;0];;

(*zadanie 2*)
let separator = " ";;

let rec zdanie (xs, k) =
if xs <> [] then List.hd xs ^ separator ^ zdanie(List.tl xs, k)
else  Char.escaped k ;;

zdanie(["poczatek";"koniec"], '!');;
zdanie(["wyraz_a";"wyraz_b";"wyraz_c"], '.');;
zdanie([], '?');;

(*zadanie 3*)
let rec dodatnie xs =
   if xs != [] then
   List.hd xs > 0 && dodatnie(List.tl xs)
   else true
;;

dodatnie [1;2;3];;
dodatnie [1];;
dodatnie [];;
dodatnie [1;2;-1];;
dodatnie [0;0;0];;
dodatnie [-1;-2;-3];;

(*zadanie 4*)
let rec silnia n =
  if n > 0 then n * silnia(n-1)
  else if n=0 then 1
  else failwith "Ujemny argument"
;;

silnia(10);;
silnia(1);;
silnia(0);;
silnia(-1);;
