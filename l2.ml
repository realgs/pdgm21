let rec sum xs =
    if xs = [] then 0
    else (List.hd xs) + sum (List.tl xs);;

sum([1;2;3;4]) = 10;;
sum([]) = 0;;
sum([1;-1;2]) = 2;;
