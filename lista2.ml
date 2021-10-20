(*Zadanie 1*)
let rec sum xs =
if (xs = []) then 0
else List.hd xs + sum(List.tl xs);;

sum([1; 4; 6; 2]);;
sum([]);;
sum([-2; -4; 5; 3]);;

(*Zadanie 2*)
let space = " ";;

let rec writer (xs,x) =
if (xs = []) then x
else List.hd xs ^
    if List.tl xs = [] then writer(List.tl xs, x)
    else space ^ writer(List.tl xs, x);;

writer(["Ala"; "ma"; "kota"], ".");;

(*Zadanie 3*)
let rec bigerThanZero xs =
if (xs = []) then true
else if (List.hd xs < 0) then false
else bigerThanZero(List.tl xs);;

bigerThanZero([1; 5; 8; 3]);;
bigerThanZero([1; 5; -8; 3]);;
bigerThanZero([]);;

(*Zadanie 4*)
let rec silnia n =
if n = 0 then 1
else if n > 0 then n * silnia(n - 1)
else failwith "Ujemna wartosc warunku! (n < 0)";;

silnia(0);;
silnia(-2);;
silnia(4);;

