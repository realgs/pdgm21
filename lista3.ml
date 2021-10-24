(*Zadanie 1*)
let twoListCreator xs =
    let rec recTwoListCreator xs l1 l2 =
        if xs = [] then (l1, l2)
        else if List.hd xs < 0 then recTwoListCreator (List.tl xs) (List.hd xs :: l1) l2
        else if List.hd xs mod 2 = 1 then recTwoListCreator(List.tl xs) l1 (List.hd xs :: l2)
        else recTwoListCreator (List.tl xs) l1 l2
    in recTwoListCreator xs [] [];;

twoListCreator [-3; -6; 7; -9; 13];;
twoListCreator [-3; -6; 7; -9; 13; 4; 9];;
twoListCreator [8; 10; 4; 12];;


(*Zadanie 2*)
let rec lengthOfList (xs) =
    if (xs = []) then 0
    else 1 + lengthOfList (List.tl xs);;

lengthOfList(["2";"1";"3";"7";"4";"2";"0"]) = 7;;
lengthOfList([]) = 0;;

(*Zadanie 3*)
let zip (xs, ys) =
    let rec recZip (xs, ys, resultList) =
        if xs = [] then resultList @ ys
        else if ys = [] then resultList @ xs
        else recZip (ys, List.tl xs, resultList @ [List.hd xs])
    in recZip (xs, ys, []);;

zip([5;4;3;2], [1;2;3;4;5;6]);;
zip(['a';'b';'c'], []);;
zip([], [2;7;14;17]);;
zip([],[]);;
