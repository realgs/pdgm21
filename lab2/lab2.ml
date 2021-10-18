
(*Zadanie 1*)
let rec sum xs = 
    if xs <> [] then List.hd xs + sum(List.tl xs)
    else 0
;;
sum[1;2;3;4;5] = 15;;
sum[] = 0;;
sum[1] = 1;;

(*zadanie 2*)
let rec str(xs,chend) = 
    if xs <> [] then
        List.hd xs ^ " " ^ str(List.tl xs)
    else chend
;; 
str[] = "."
str["Ala";"ma";"Kota"] = "Ala ma Kota ."

(*Zadanie 3*)
let rec more_then_0 xs =
    if xs <> [] then 
        if List.hd xs > 0 then more_then_0(List.tl xs)
        else false
    else  true
;;
more_then_0[] = true;;
more_then_0[9;5;6;-9] = false;; 
more_then_0[1;2;3;4] = true;;

(*zadanie 4*)
let rec silnia x = 
    if x = 0 then 1 else x*silnia(x-1)
;;
silnia 1;;
silnia 0;;
silnia 10;;
