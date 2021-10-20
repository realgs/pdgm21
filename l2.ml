let rec sum xs =
    if xs = [] then 0
    else (List.hd xs) + sum (List.tl xs);;

sum([1;2;3;4]) = 10;;
sum([]) = 0;;
sum([1;-1;2]) = 2;;

let rec makeSentence (xs, x) =
    if xs = [] then x
    else if List.tl xs = [] then (List.hd xs) ^ x
    else (List.hd xs) ^ " " ^ makeSentence (List.tl xs, x);;

makeSentence (["Ala";"ma";"kota"], ".") = "Ala ma kota.";;
makeSentence (["ya";"like";"jazz"], "?") = "ya like jazz?";;
makeSentence ([], "!") = "!";;
