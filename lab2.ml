(*1*)
let rec sumOfList xs =
    if xs = [] then 0
    else List.hd xs + sumOfList(List. tl xs);;

sumOfList[] = 0;;
sumOfList[1; 2; 3; 4] = 10;;
sumOfList[0; -1] = -1;;

(*2*)
let rec sentence (xs, x) =
    if xs = [] then x
    else if List.tl xs = [] then List.hd xs ^ x
    else List.hd xs ^ " " ^ sentence(List.tl xs, x);;

sentence(["How"; "are"; "you"], "?") = "How are you?";;
sentence([], ".") = ".";;
sentence(["Dog"], ";") = "Dog;";;

(*3*)

let rec checkList xs =
    if xs = [] then true
    else if List.hd xs > 0 then checkList(List.tl xs)
    else false;;

checkList[1; 0; -2; 4] = false;;
checkList[1; 2; 3; 4] = true;;
checkList[] = true;;

(*4*)
let rec power n =
    if n = 0 then 1
    else if n > 0 then n * power(n - 1)
    else failwith "invalid number";;

power(3) = 6;;
power(0) = 1;;
(*power(-1);;*)
